#!/usr/bin/env bash
# Regression: a worker run blocked on a read writes nothing.
# watched_scryer/1 stops a run that writes nothing for
# WORKER_IDLE_TIMEOUT seconds, and any run after WORKER_TIMEOUT, and
# says so; runs that keep writing are left alone.
set -uo pipefail

cd "$(dirname "$0")/.." || exit 1
# shellcheck source=../fun.sh
. ./fun.sh
set +e

failures=0
check() {
    local label="$1" expected="$2" actual="$3"
    if [ "${expected}" = "${actual}" ]; then
        echo "[OK] watched_scryer: ${label}"
    else
        echo "[KO] watched_scryer: ${label}: expected ${expected}, got ${actual}"
        failures=$((failures + 1))
    fi
}

# Stand-in for scryer-prolog: runs the shell snippet given as its argument.
fake="$(mktemp -d)"
printf '#!/usr/bin/env bash\neval "$1"\n' > "${fake}/scryer-prolog"
chmod +x "${fake}/scryer-prolog"
export PATH="${fake}:${PATH}"

started=$(date +%s)
out="$(WORKER_IDLE_TIMEOUT=2 WORKER_TIMEOUT=60 watched_scryer 'echo working; sleep 30; echo late' 2>&1)"
rc=$?
check "a silent run is stopped" 124 "${rc}"
check "a silent run is stopped after the idle limit, not later" true "$([ $(( $(date +%s) - started )) -lt 15 ] && echo true || echo false)"
check "its output so far is kept" true "$(grep -q '^working$' <<<"${out}" && echo true || echo false)"
check "the stop is labelled" true "$(grep -q 'worker_stalled(idle_timeout(2))' <<<"${out}" && echo true || echo false)"
check "nothing runs after the stop" false "$(grep -q '^late$' <<<"${out}" && echo true || echo false)"

out="$(WORKER_IDLE_TIMEOUT=2 WORKER_TIMEOUT=60 watched_scryer 'for i in 1 2 3 4 5; do echo "step $i"; sleep 1; done' 2>&1)"
rc=$?
check "a run that keeps writing past the idle limit completes" 0 "${rc}"
check "all of its output comes through" 5 "$(grep -c '^step ' <<<"${out}")"

WORKER_IDLE_TIMEOUT=2 WORKER_TIMEOUT=60 watched_scryer 'echo failing; exit 3' >/dev/null 2>&1
check "the run's exit status is returned" 3 "$?"

started=$(date +%s)
out="$(WORKER_IDLE_TIMEOUT=60 WORKER_TIMEOUT=3 watched_scryer 'while true; do echo busy; sleep 0.5; done' 2>&1)"
rc=$?
check "a run is stopped at the overall limit" 124 "${rc}"
check "the overall stop is labelled" true "$(grep -q 'worker_stalled(timeout(3))' <<<"${out}" && echo true || echo false)"
check "the overall limit is kept to" true "$([ $(( $(date +%s) - started )) -lt 20 ] && echo true || echo false)"

rm -rf "${fake}"
[ "${failures}" -eq 0 ]
