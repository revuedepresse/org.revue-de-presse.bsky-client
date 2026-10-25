#!/usr/bin/env bash
# Every shell-out must go through os_ext:shell_or_throw/3, which gives
# scryer's shell/2 a SHELL and turns its silent failure into a throw.
# A direct shell/2 call anywhere else in src/ brings the silent failure
# back, so this check fails on any `shell(` call outside that wrapper.
#
# Usage: bash tests/no_direct_shell_call_test.sh (from the repository root).
set -Euo pipefail

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
spec='no_direct_shell_call:every_shell_out_goes_through_shell_or_throw'

# `shell(` not preceded by an identifier character, outside % comments.
direct_calls="$(
    cd "${repository_root}" &&
        grep -rnE '(^|[^A-Za-z0-9_])shell\(' src --include='*.pl' |
        grep -vE '^[^:]+:[0-9]+:[[:space:]]*%' |
        grep -vE '^src/os_ext\.pl:[0-9]+:[[:space:]]*shell\(Command, Status\)\.$' || true
)"
wrapper_calls="$(cd "${repository_root}" && grep -cE '^[[:space:]]*shell\(Command, Status\)\.$' src/os_ext.pl || true)"

if [ -z "${direct_calls}" ] && [ "${wrapper_calls}" = 1 ]; then
    printf '[OK] %s\n' "${spec}"
else
    printf '[KO] %s\n' "${spec}"
    [ "${wrapper_calls}" = 1 ] || printf 'expected one shell/2 call in os_ext:shell_or_unavailable/3, behind shell_or_throw/3, found %s\n' "${wrapper_calls}"
    [ -z "${direct_calls}" ] || printf 'direct shell/2 calls:\n%s\n' "${direct_calls}"
    exit 1
fi
