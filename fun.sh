#!/usr/bin/env bash
set -Eeuo pipefail

function configure() {
    local name
    local value

    for envar in $(env | grep -vE '^(SHELL|HOME|USER|PATH|ACTOR|TEXT)' | cut -f 1 -d '='); do
        unset "${envar}"
    done

    for env_var in $(\cat ./.env.local | grep -vE '^#'); do
        name="$(echo ${env_var} | cut -d '=' -f 1)"
        value="$(echo ${env_var} | cut -d '=' -f 2)"
        export "${name}"="${value}"
    done
}

function com__atproto__server__createSession() {
    configure

    local access_jwt
    access_jwt='write('"'access jwt: '"'),atom_chars(AtomicAccessJwt,AccessJwt),write(AtomicAccessJwt),nl'

    local refresh_jwt
    refresh_jwt='write('"'refresh jwt: '"'),atom_chars(AtomicRefreshJwt,RefreshJwt),write(AtomicRefreshJwt),nl'

    scryer -g 'com__atproto__server__createSession(AccessJwt, RefreshJwt),'"${access_jwt},${refresh_jwt}" \
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

    scryer -g 'com__atproto__repo__createRecord("'"${text}"'", Props)' \
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

    scryer -g 'app__bsky__actor__getProfile("'"${actor}"'", Prop)' \
        ./src/app/bsky/actor/getProfile.pl
}

function list_endpoints() {
    \cat ./doc/api.json | \
    jq '.paths' | \
    jq keys | \
    grep -v 'unspecced\|ozone' | \
    grep '"/' | \
    tr -d ',' | \
    tr -d '"' | \
    sed -E 's#/#.#g'
}

function list_namespaces() {
    list_endpoints | \
    sed -E 's#(.+)\.[^.]*$#\1#g' | \
    tr '.' '/' | \
    uniq | \
    xargs -I{} sh -c 'echo ./src${1}' shell {}
}

function list_accessors() {
  local cmd
  cmd='echo /src${1}'
  cmd="${cmd}"' | tr "." "/"'
  cmd="${cmd}"' | sed -E "s#^#.#g"'
  cmd="${cmd}"' | sed -E "s#(.*)#\1.pl#g"'

  make list-endpoints | \
  xargs -I{} sh -c "${cmd}" shell {}
}

set +Eeuo pipefail
