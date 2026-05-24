#!/usr/bin/env bash
# Concurrent SIGSEGV reproducer orchestrator.
#
# Spawns N parallel scryer-prolog workers (default 3, matching
# production's `get_authors_feeds` in
# ~deployer/.maintaining/org.revue-de-presse.bsky.sh), each replaying
# a different captured `last-feed-pairs.pl` fixture against the
# production pg_query path. Watches for the first exit_code=139 --
# that is the reproduction signal for the patched-VM SIGSEGV the
# upstream b02e0c47 guard does not catch.
#
# Inputs (env / overridable)
# --------------------------
#   PARALLEL            number of concurrent workers (default 3)
#   REPRODUCER_ROUNDS   rounds per worker  (default 20)
#   SCRYER_BIN          patched scryer-prolog binary
#                       (default deps/scryer-prolog/target/release/scryer-prolog)
#   FIXTURES            newline-separated fixture paths. Default: the
#                       N most-recent patched-VM captures pulled into
#                       var/tmp/segv-investigation-pull/.
#
# DB env comes from .env.local.

set -Eeuo pipefail

cd "$(dirname "$0")/../.."
REPO_ROOT="$(pwd -P)"
REPRO_PL="$REPO_ROOT/tests/pg/concurrent_segv_reproducer.pl"

# Enable coredumps for this shell + every child process we spawn.
# Container-side ulimit may already be unlimited (compose `ulimits.core`),
# but on bare metal this is the actual lever.
ulimit -c unlimited 2>/dev/null || true

# shellcheck disable=SC1091
set -a; . ./.env.local; set +a

# Where the kernel writes coredumps. Inside the docker container this is
# bind-mounted from the host so dumps survive container exit; on bare
# metal it falls back to wherever the host's kernel.core_pattern points.
COREDUMP_DIR="${COREDUMP_DIR:-/coredumps}"
if [ ! -d "$COREDUMP_DIR" ]; then
    COREDUMP_DIR=""  # let the kernel pattern decide
fi

SCRYER_BIN="${SCRYER_BIN:-deps/scryer-prolog/target/release/scryer-prolog}"
if [ ! -x "$SCRYER_BIN" ]; then
    echo "[KO] scryer binary not built: $SCRYER_BIN" >&2
    echo "     run 'make scryer-prolog-build' (or 'cargo build --release' under deps/scryer-prolog/)" >&2
    exit 2
fi

PARALLEL="${PARALLEL:-3}"
REPRODUCER_ROUNDS="${REPRODUCER_ROUNDS:-20}"
export REPRODUCER_ROUNDS

if [ -z "${FIXTURES:-}" ]; then
    # Prefer the pulled-locally tree; fall back to the in-place
    # production tree so the same script can run on caprica without
    # re-pulling.
    FIXTURES=$(
        for src in var/tmp/segv-investigation-pull var/tmp/segv-investigation; do
            [ -d "$src" ] || continue
            find "$REPO_ROOT/$src" -maxdepth 2 -name last-feed-pairs.pl 2>/dev/null
        done \
        | while read -r f; do
            d=$(dirname "$f")
            if grep -q "v0.10.0-186" "$d/manifest.txt" 2>/dev/null; then
                printf '%s\n' "$f"
            fi
        done \
        | sort \
        | tail -n "$PARALLEL"
    )
fi

if [ -z "$FIXTURES" ]; then
    echo "[KO] no patched-VM fixtures found under var/tmp/segv-investigation-pull/" >&2
    echo "     pull captures from io.marianne.caprica first" >&2
    exit 2
fi

N_FIXTURES=$(printf '%s\n' "$FIXTURES" | wc -l | tr -d ' ')
if [ "$N_FIXTURES" -lt "$PARALLEL" ]; then
    echo "[..] only $N_FIXTURES fixtures available; reducing PARALLEL from $PARALLEL to $N_FIXTURES"
    PARALLEL="$N_FIXTURES"
fi

RUN_TS=$(date -u +%Y%m%dT%H%M%SZ)
RUN_DIR="var/tmp/reproducer-runs/run-$RUN_TS"
mkdir -p "$RUN_DIR"

echo "[..] reproducer run: $RUN_DIR"
echo "[..] scryer:         $SCRYER_BIN"
echo "[..] parallel:       $PARALLEL workers"
echo "[..] rounds/worker:  $REPRODUCER_ROUNDS"
echo "[..] fixtures:"
printf '       %s\n' "$FIXTURES"
echo ""

declare -a PIDS=()
declare -a LOGS=()
declare -a TAGS=()
declare -a FIXTS=()

i=0
while IFS= read -r fixture; do
    [ -z "$fixture" ] && continue
    i=$((i + 1))
    tag="w$i"
    log="$RUN_DIR/$tag.log"
    work_dir="$RUN_DIR/$tag"
    mkdir -p "$work_dir"
    LOGS+=("$log")
    TAGS+=("$tag")
    FIXTS+=("$fixture")
    # Each worker runs with its own cwd so the kernel's default
    # `core_pattern=core` writes per-worker dump files that do not
    # collide on concurrent crashes -- no host sysctl change needed.
    (
        cd "$work_dir"
        ulimit -c unlimited 2>/dev/null || true
        export FEED_PAIRS_FIXTURE="$fixture"
        export REPRODUCER_TAG="$tag"
        export RUST_BACKTRACE=full
        exec "$SCRYER_BIN" "$REPRO_PL" -g run_test
    ) > "$log" 2>&1 &
    PIDS+=($!)
done <<< "$FIXTURES"

echo "[..] launched PIDs: ${PIDS[*]}"
echo "[..] waiting..."
echo ""

REPRO_TAGS=()
for idx in "${!PIDS[@]}"; do
    pid=${PIDS[$idx]}
    tag=${TAGS[$idx]}
    log=${LOGS[$idx]}
    fix=${FIXTS[$idx]}
    if wait "$pid"; then
        rc=0
    else
        rc=$?
    fi
    printf '[%s] pid=%s rc=%s fixture=%s\n' "$tag" "$pid" "$rc" "$(basename "$(dirname "$fix")")"
    if [ "$rc" -eq 139 ]; then
        echo "    *** SIGSEGV REPRODUCED ($tag) ***"
        REPRO_TAGS+=("$tag")
    fi
    if [ -s "$log" ]; then
        echo "    last 3 log lines:"
        tail -3 "$log" | sed 's/^/      /'
    fi
done

# Report any coredumps the kernel wrote.
echo ""
echo "[..] scanning for coredumps..."
DUMP_FOUND=0
for d in "$COREDUMP_DIR" "$RUN_DIR" "$(pwd)"; do
    [ -z "$d" ] && continue
    [ -d "$d" ] || continue
    while IFS= read -r dump; do
        DUMP_FOUND=1
        sz=$(stat -c%s "$dump" 2>/dev/null || stat -f%z "$dump" 2>/dev/null || echo "?")
        echo "     coredump: $dump  (size=$sz bytes)"
    done < <(find "$d" -maxdepth 2 -newer "$RUN_DIR" \( -name 'core' -o -name 'core.*' -o -name '*.core' \) 2>/dev/null)
done
if [ "$DUMP_FOUND" -eq 0 ]; then
    echo "     (no coredumps found)"
fi

echo ""
if [ ${#REPRO_TAGS[@]} -gt 0 ]; then
    echo "[OK] SIGSEGV reproduced on: ${REPRO_TAGS[*]}"
    echo "     logs under $RUN_DIR/"
    if [ "$DUMP_FOUND" -ne 0 ]; then
        echo "     Analyse a dump with:"
        echo "       docker compose -f compose.repro.yaml run --rm segv-gdb"
        echo "       (inside) gdb /usr/local/bin/scryer-prolog /coredumps/<dump>"
    fi
    exit 0
fi

echo "[..] no SIGSEGV across $PARALLEL workers x $REPRODUCER_ROUNDS rounds"
echo "     concurrency-alone hypothesis not confirmed locally"
echo "     next step to try: install cpulimit (brew install cpulimit) and re-run"
echo "     with each worker wrapped in 'cpulimit -l 30' to mimic production"
exit 1
