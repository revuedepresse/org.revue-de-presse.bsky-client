#!/usr/bin/env bash
set -Eeuo pipefail

function configure() {
    local name
    local value

    for envar in $(env | grep -vE '^(SHELL|HOME|USER|PATH|ACTOR|AUTHOR|TEXT|RUST_BACKTRACE|RUST_LIB_BACKTRACE)' | cut -f 1 -d '='); do
        unset "${envar}"
    done

    for env_var in $(\cat ./.env.local | grep -vE '^#'); do
        name="$(echo ${env_var} | cut -d '=' -f 1)"
        value="$(echo ${env_var} | cut -d '=' -f 2)"
        export "${name}"="${value}"
    done

    # Surface any Rust-side panic backtrace so a SIGSEGV or panic in
    # scryer-prolog (or its FFI deps like postgresql-prolog) can be
    # captured in worker logs and pasted into upstream issues / PRs.
    # SIGSEGV itself bypasses the panic handler; the env var still
    # helps when the crash is reachable via panic_unwind first.
    export RUST_BACKTRACE="${RUST_BACKTRACE:-full}"
    export RUST_LIB_BACKTRACE="${RUST_LIB_BACKTRACE:-full}"

    # Prefer the in-tree patched scryer-prolog binary if it has been
    # built. deps/scryer-prolog ships ahead of the published release
    # by the commit `b02e0c47 fix: Guard Unifier::unify_constant
    # against bogus arena pointers` -- which is the fix for the
    # EXC_BAD_ACCESS @ 0x301c7 SIGSEGV that takes down the worker
    # mid-iteration of the getAuthorFeed maplist on the legacy
    # v0.10.0-96 binary. Falling back to whatever scryer-prolog is
    # already on PATH keeps fresh checkouts working without forcing
    # a `cargo build --release` up front; the worker will SIGSEGV
    # against an unpatched binary but `classify_worker_exit` /
    # `preserve_segv_captures` will at least say so out loud.
    local patched_scryer="./deps/scryer-prolog/target/release/scryer-prolog"
    if [ -x "${patched_scryer}" ]; then
        export PATH="$(cd "$(dirname "${patched_scryer}")" && pwd):${PATH}"
    fi
}

function com__atproto__server__createSession() {
    configure

    local access_jwt
    access_jwt='write('"'access jwt: '"'),atom_chars(AtomicAccessJwt,AccessJwt),write(AtomicAccessJwt),nl'

    local refresh_jwt
    refresh_jwt='write('"'refresh jwt: '"'),atom_chars(AtomicRefreshJwt,RefreshJwt),write(AtomicRefreshJwt),nl'

    scryer-prolog -g 'com__atproto__server__createSession(AccessJwt, RefreshJwt),'"${access_jwt},${refresh_jwt}" \
        ./src/com/atproto/server/createSession.pl
}

function com__atproto__repo__createRecord() {
    configure

    local env_var_input
    env_var_input=${TEXT:-}

    local text
    text="${1:-${env_var_input}}"

    if [ -z "${text}" ]; then
        printf 'A %s is expected as %s (%s).%s' 'non-empty string' '1st argument' 'Post text' $'\n'
        return 1
    fi

    scryer-prolog \
        -g 'com__atproto__repo__createRecord("'"${text}"'", Props)' \
        -g halt \
        ./src/com/atproto/repo/createRecord.pl
}

# https://docs.bsky.app/docs/api/app-bsky-actor-get-profile
function app__bsky__actor__getProfile() {
    configure

    local env_var_input
    env_var_input=${ACTOR:-}

    local actor
    actor="${1:-${env_var_input}}"

    if [ -z "${actor}" ]; then
        printf 'A %s is expected as %s (%s).%s' 'non-empty string' '1st argument' 'Handle or DID of account to fetch profile of.' $'\n'
        return 1
    fi

    scryer-prolog \
        -g 'app__bsky__actor__getProfile("'"${actor}"'", Prop)' \
        -g halt \
        ./src/app/bsky/actor/getProfile.pl
}

# https://docs.bsky.app/docs/api/app-bsky-feed-get-author-feed
function app__bsky__feed__getAuthorFeed() {
    configure

    local env_var_input
    env_var_input=${AUTHOR:-}

    local author
    author="${1:-${env_var_input}}"

    if [ -z "${author}" ]; then
        printf 'A %s is expected as %s (%s).%s' 'non-empty string' '1st argument' 'Handle or DID of author to fetch feed of.' $'\n'
        return 1
    fi

    mkdir -p /tmp/segv-investigation var/tmp/segv-investigation

    # Enable coredumps for the scryer process. With the host's default
    # kernel.core_pattern=core the kernel drops a `core` file in the
    # process's cwd on SIGSEGV; preserve_segv_captures picks it up
    # alongside the Prolog-side captures. Clear any stale core from a
    # previous run first so we don't archive the wrong dump.
    ulimit -c unlimited 2>/dev/null || true
    rm -f ./core 2>/dev/null || true

    set +e
    scryer-prolog \
        -g 'app__bsky__feed__getAuthorFeed("'"${author}"'", Prop).' \
        -g halt \
        ./src/app/bsky/feed/getAuthorFeed.pl
    local rc=$?
    set -e

    if [ "${rc}" -ne 0 ]; then
        preserve_segv_captures "${author}" "${rc}"
    fi

    return "${rc}"
}

# When a process is killed by a signal, the shell reports rc = 128 + signo.
# This is a UNIX convention -- a clean prolog failure stays in 1..127 and a
# signal kill (SIGSEGV = 11 -> 139, SIGABRT = 6 -> 134, SIGKILL = 9 -> 137)
# lands above 128. We surface that boundary as a labelled error so cron /
# supervisor / operator logs can grep `worker_crashed_by_signal=` and
# distinguish a VM crash from "the predicate returned false".
function classify_worker_exit() {
    local rc=$1
    if [ "${rc}" -ge 128 ]; then
        local signo=$((rc - 128))
        local signame
        signame=$(kill -l "${signo}" 2>/dev/null || echo "UNKNOWN")
        printf 'worker_crashed_by_signal=%d signal_name=SIG%s exit_code=%d\n' \
            "${signo}" "${signame}" "${rc}"
    else
        printf 'worker_failed_cleanly exit_code=%d\n' "${rc}"
    fi
}

# Snapshot whatever the Prolog-side capture writers left under
# /tmp/segv-investigation into var/tmp/segv-investigation/crash-<ts>-pid<n>/
# the moment the worker exits non-zero. Production cron / supervisor
# restarts the worker and the next call would otherwise overwrite the
# captured terms, so we copy them out into a project-local, persistent
# location with a manifest. var/tmp/* is gitignored.
function preserve_segv_captures() {
    local author=$1
    local rc=$2
    local ts
    ts=$(date -u +%Y%m%dT%H%M%SZ)
    local dest="var/tmp/segv-investigation/crash-${ts}-pid$$"
    local classification
    classification=$(classify_worker_exit "${rc}")
    mkdir -p "${dest}"
    {
        printf 'timestamp_utc=%s\n' "${ts}"
        printf 'exit_code=%d\n' "${rc}"
        printf '%s\n' "${classification}"
        printf 'author=%s\n' "${author}"
        printf 'host=%s\n' "$(hostname)"
        printf 'scryer_version=%s\n' "$(scryer-prolog --version 2>/dev/null || echo unknown)"
        printf 'git_head=%s\n' "$(git rev-parse --short HEAD 2>/dev/null || echo unknown)"
    } > "${dest}/manifest.txt"
    local f
    for f in last-by-indexed-at.pl last-feed-body.txt last-feed-pairs.pl; do
        if [ -f "/tmp/segv-investigation/${f}" ]; then
            cp "/tmp/segv-investigation/${f}" "${dest}/"
        fi
    done

    # If the kernel left a coredump in cwd (because ulimit -c was
    # raised before the scryer launch), move it into the capture
    # dir and record which binary it corresponds to so a later gdb
    # session can resolve symbols against the right scryer build.
    if [ -f ./core ]; then
        mv ./core "${dest}/coredump"
        readlink -f "$(command -v scryer-prolog 2>/dev/null)" \
            > "${dest}/coredump.binary.txt" 2>/dev/null || true
    fi

    echo "[crash-capture] ${classification} captures_at=${dest}" >&2
}

# https://docs.bsky.app/docs/api/app-bsky-graph-get-list
function app__bsky__graph__getList() {
    configure

    local env_var_input
    env_var_input=${LIST_AT_URI:-}

    local at_uri
    at_uri="${1:-${env_var_input}}"

    if [ -z "${at_uri}" ]; then
        printf 'A %s is expected as %s (%s).%s' 'non-empty string' '1st argument' 'Reference (AT-URI) of the list record to hydrate.' $'\n'
        return 1
    fi

    scryer-prolog \
        -g 'app__bsky__graph__getList("'"${at_uri}"'", Prop).' \
        -g 'halt.' \
        ./src/app/bsky/graph/getList.pl
}

function infrastructure__list__count() {
    configure

    scryer-prolog \
        -g 'count(Count), writeq(Count).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_list.pl
}

function infrastructure__list__query() {
    configure

    scryer-prolog \
        -g 'query(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_list.pl
}

function infrastructure__list__next_event_id() {
    configure

    scryer-prolog \
        -g 'next_event_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_list.pl
}

function infrastructure__list__next_id() {
    configure

    scryer-prolog \
        -g 'next_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_list.pl
}

function infrastructure__list_item__count() {
    configure

    scryer-prolog \
        -g 'count(Count), writeq(Count)' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_list_item.pl
}

function infrastructure__list_item__query() {
    configure

    scryer-prolog \
        -g 'query(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_list_item.pl
}

function infrastructure__list_item__next_id() {
    configure

    scryer-prolog \
        -g 'next_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_list_item.pl
}

function infrastructure__publisher__count() {
    configure

    scryer-prolog \
        -g 'count(Count), writeq(Count)' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publisher.pl
}

function infrastructure__publisher__query() {
    configure

    scryer-prolog \
        -g 'query(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publisher.pl
}

function infrastructure__publisher__next_id() {
    configure

    scryer-prolog \
        -g 'next_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publisher.pl
}

function infrastructure__status__count() {
    configure

    scryer-prolog \
        -g 'count(Count), writeq(Count).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_status.pl
}

function infrastructure__status__query() {
    configure

    scryer-prolog \
        -g 'query(Result), write_term(Result, [double_quotes(true)]).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_status.pl
}

function infrastructure__status__next_id() {
    configure

    scryer-prolog \
        -g 'next_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_status.pl
}

function infrastructure__popularity__count() {
    configure

    scryer-prolog \
        -g 'count(Count), writeq(Count).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_popularity.pl
}

function infrastructure__popularity__query() {
    configure

    scryer-prolog \
        -g 'query(Result), write_term(Result, [double_quotes(true)]).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_popularity.pl
}

function infrastructure__popularity__next_id() {
    configure

    scryer-prolog \
        -g 'next_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_popularity.pl
}

function infrastructure__publication__count() {
    configure

    scryer-prolog \
        -g 'count(Count), writeq(Count).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publication.pl
}

function infrastructure__publication__query() {
    configure

    scryer-prolog \
        -g 'query(Result), write_term(Result, [double_quotes(true)]).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publication.pl
}

function infrastructure__publication__next_id() {
    configure

    scryer-prolog \
        -g 'next_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publication.pl
}

# https://docs.bsky.app/docs/api/app-bsky-graph-get-lists
function app__bsky__graph__getLists() {
    configure

    local env_var_input
    env_var_input=${ACTOR:-}

    local actor
    actor="${1:-${env_var_input}}"

    if [ -z "${actor}" ]; then
        printf 'A %s is expected as %s (%s).%s' 'non-empty string' '1st argument' 'Handle or DID of account to fetch profile of.' $'\n'
        return 1
    fi

    scryer-prolog -g 'app__bsky__graph__getLists("'"${actor}"'", Prop)' \
        ./src/app/bsky/graph/getLists.pl -g halt
}

function list_endpoints() {
    \cat ./doc/api.json |
        jq '.paths' |
        jq 'keys[]' |
        grep -v 'unspecced\|ozone' |
        grep '"/' |
        tr -d ',' |
        tr -d '"' |
        sed -E 's#/#.#g'
}

function list_namespaces() {
    list_endpoints |
        sed -E 's#(.+)\.[^.]*$#\1#g' |
        tr '.' '/' |
        uniq |
        xargs -I{} sh -c 'echo ./src${1}' shell {}
}

function list_accessors() {
    local cmd
    cmd='echo /src${1}'
    cmd="${cmd}"' | tr "." "/"'
    cmd="${cmd}"' | sed -E "s#^#.#g"'
    cmd="${cmd}"' | sed -E "s#(.*)#\1.pl#g"'

    make list-endpoints |
        xargs -I{} sh -c "${cmd}" shell {}
}

function list_api_spec_keys() {
    local cmd
    cmd='echo ${1} > $(echo ${1}'
    # shellcheck disable=SC2089
    cmd="${cmd}"' | sed -E "s#/#./#g"'
    cmd="${cmd}"' | sed -E "s#(.*)#doc/endpoints/\1.key.json#g")'

    \cat ./doc/api.json |
        jq '.paths | to_entries | .[] | .key' |
        xargs -I{} sh -c "${cmd}" shell {}
}

function list_api_spec_values() {
    local value
    # shellcheck disable=SC2045
    for i in $(ls -1 ./doc/endpoints/*key.json); do
        value="$(echo $i | sed -E 's#key#value#')"
        jq ".paths | $(echo '."'"$(\cat $i)"'"')" ./doc/api.json \
            >"${value}"
    done
}

function test() {
    if ! ls ./tests/types/string/formats/*_test.pl >/dev/null 2>&1; then
        printf '%s.%s' 'No string-format tests found at ./tests/types/string/formats/' $'\n'
        exit 1
    fi
    scryer-prolog \
        ./tests/types/string/formats/*_test.pl \
        ./tests/memoize_arity_test.pl \
        ./tests/iterate_or_report_failure_test.pl \
        ./tests/feed_capture_replay_test.pl \
        ./tests/clean_text_test.pl \
        -g halt |
        tee ./test.log
    # shellcheck disable=SC2046
    if [ $(grep -c '\[KO\]' ./test.log) -gt 0 ]; then
        printf '%s.%s' 'Some tests failed' $'\n'
        rm ./test.log
        exit 1
    else
        printf '%s.%s' 'All tests passed successfully' $'\n'
    fi
    rm ./test.log
}

# --- staging database helpers -----------------------------------------
#
# Procedures backing the `make staging-db-*` targets. They are kept
# here (rather than inlined in the Makefile) so the same shell logic
# is callable from a regular shell: `bash -c '. ./fun.sh && staging_db_create'`.
#
# DATABASE_* credentials come from ./.env.local via staging_db_load_env;
# the staging DB name is hardcoded to match the SQL files under
# var/migrations/2026-05-25-staging-*.sql.

function staging_db_name() {
    printf '%s' 'org_revue_de_presse_staging'
}

# Source DATABASE_* (and everything else) from ./.env.local into the
# current shell so the psql wrapper below has the credentials it needs.
# Uses `set -a` to auto-export each assignment without unsetting the
# parent shell's unrelated env vars (`configure` does that for the
# scryer runs; we don't need that here).
function staging_db_load_env() {
    if [ ! -f ./.env.local ]; then
        echo "[staging-db] missing ./.env.local; run from the repo root" >&2
        return 1
    fi
    set -a
    # shellcheck disable=SC1091
    . ./.env.local
    set +a
}

# psql wrapper that picks the connection from .env.local and applies
# the same flags every staging-db helper wants: ON_ERROR_STOP=1 so the
# Makefile (or caller) sees a non-zero exit on the first SQL error.
function staging_db_psql() {
    local db="$1"
    shift
    staging_db_load_env
    PGPASSWORD="${DATABASE_PASSWORD}" psql \
        -h "${DATABASE_HOST}" -p "${DATABASE_PORT}" \
        -U "${DATABASE_USERNAME}" -d "${db}" \
        -v ON_ERROR_STOP=1 "$@"
}

function staging_db_create() {
    staging_db_psql postgres \
        -f var/migrations/2026-05-25-staging-create-db.sql
}

function staging_db_schema() {
    staging_db_psql "$(staging_db_name)" \
        -f var/migrations/2026-05-25-staging-schema.sql
}

function staging_db_drop() {
    staging_db_psql postgres \
        -c "DROP DATABASE IF EXISTS $(staging_db_name);"
}

function staging_db_truncate() {
    staging_db_psql "$(staging_db_name)" \
        -c "TRUNCATE TABLE status_popularity, publication, weaving_status RESTART IDENTITY;"
}

set +Eeuo pipefail
