#!/usr/bin/env bash
# Orchestrator for the HTTP-backed SIGSEGV reproducer.
#
# Starts tests/pg/serve_feed_fixture.py in the background, waits
# for the port to listen, runs tests/pg/sigsegv_http_repro.pl
# against it, then cleans up.
#
# Env knobs
# ---------
#   PORT          server port (default 8080)
#   JSON_PATH     reconstructed feed JSON to serve
#                 (default var/tmp/segv-investigation/
#                  crash-20260524T170851Z-pid3985412/feed-body.json)
#   POST_INDEX    0-based post index (default 0)
#   SCRYER_BIN    scryer-prolog (default: which scryer-prolog)
#   .env.local    sourced for DATABASE_*.

set -Eeuo pipefail

cd "$(dirname "$0")/../.."
REPO_ROOT="$(pwd -P)"

PORT="${PORT:-8080}"
JSON_PATH="${JSON_PATH:-var/tmp/segv-investigation/crash-20260524T170851Z-pid3985412/feed-body.json}"
POST_INDEX="${POST_INDEX:-0}"
SCRYER_BIN="${SCRYER_BIN:-$(command -v scryer-prolog || echo)}"

if [ -z "${SCRYER_BIN:-}" ] || [ ! -x "$SCRYER_BIN" ]; then
    echo "[KO] scryer-prolog not on PATH (or SCRYER_BIN not executable)" >&2
    exit 2
fi
if [ ! -f "$JSON_PATH" ]; then
    echo "[KO] feed JSON not found: $JSON_PATH" >&2
    echo "     run reconstruct_feed_json.pl first" >&2
    exit 2
fi

# shellcheck disable=SC1091
set -a; . ./.env.local; set +a

ulimit -c unlimited 2>/dev/null || true
rm -f ./core 2>/dev/null || true

echo "[..] starting feed server on http://127.0.0.1:$PORT/"
PORT="$PORT" JSON_PATH="$REPO_ROOT/$JSON_PATH" \
    python3 "$REPO_ROOT/tests/pg/serve_feed_fixture.py" > /tmp/serve_feed.log 2>&1 &
SERVER_PID=$!

cleanup() {
    if kill -0 "$SERVER_PID" 2>/dev/null; then
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT INT TERM

# Wait for the server to bind. python3 prints "[..] serving ..."
# once it's listening; we can also probe the port.
for _ in $(seq 1 50); do
    if (echo > "/dev/tcp/127.0.0.1/$PORT") 2>/dev/null; then
        break
    fi
    sleep 0.1
done
echo "[..] server ready (pid=$SERVER_PID)"

echo "[..] running reproducer against http://127.0.0.1:$PORT/feed.json"
echo ""
set +e
FEED_URL="http://127.0.0.1:$PORT/feed.json" \
POST_INDEX="$POST_INDEX" \
RUST_BACKTRACE=full \
"$SCRYER_BIN" ./tests/pg/sigsegv_http_repro.pl -g run 2>&1 | tee /tmp/http_repro_run.log
RC=${PIPESTATUS[0]}
set -e

echo ""
echo "[..] reproducer exit code: $RC"
if [ "$RC" -eq 139 ]; then
    echo "[OK] SIGSEGV reproduced via http_open path"
    if [ -f ./core ]; then
        sz=$(stat -c%s ./core 2>/dev/null || stat -f%z ./core 2>/dev/null || echo "?")
        echo "[OK] coredump: $(pwd)/core ($sz bytes)"
    fi
fi
exit "$RC"
