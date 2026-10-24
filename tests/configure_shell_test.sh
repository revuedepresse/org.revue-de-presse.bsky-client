#!/usr/bin/env bash
# Scryer's shell/2 runs commands through $SHELL and fails silently when
# it is not in the environment, as under a systemd unit. configure must
# leave every scryer launch with an exported SHELL.
#
# Bash assigns SHELL when it starts without one but does not export it,
# so the check reads the environment of a child process.
#
# Usage: bash tests/configure_shell_test.sh (from the repository root).
set -Euo pipefail

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
work_dir="$(mktemp -d)"
trap 'rm -rf "${work_dir}"' EXIT
# configure reads ./.env.local: an empty one keeps the test hermetic.
: >"${work_dir}/.env.local"

failures=0

# child_shell(+LauncherEnvArgs...) prints SHELL as a process started
# after configure sees it, or <unset>.
function child_shell() {
    (cd "${work_dir}" &&
        env "$@" bash -c '. "$0/fun.sh" && configure && (printenv SHELL || echo "<unset>")' "${repository_root}")
}

function expect() {
    local spec="${1}"
    local expected="${2}"
    local actual="${3}"
    if [ "${actual}" = "${expected}" ]; then
        printf '[OK] %s\n' "${spec}"
    else
        printf '[KO] %s\nexpected: %s\nactual: %s\n' "${spec}" "${expected}" "${actual}"
        failures=$((failures + 1))
    fi
}

expect 'configure_shell:defaults_shell_to_bin_sh_when_the_launcher_has_none' \
    /bin/sh "$(child_shell -u SHELL)"
expect 'configure_shell:keeps_the_shell_the_launcher_exports' \
    /bin/bash "$(child_shell SHELL=/bin/bash)"

[ "${failures}" -eq 0 ]
