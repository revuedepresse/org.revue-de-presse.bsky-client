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
    env_var_input=${ACTOR:-}

    local author
    author="${1:-${env_var_input}}"

    if [ -z "${author}" ]; then
        printf 'A %s is expected as %s (%s).%s' 'non-empty string' '1st argument' 'Handle or DID of author to fetch feed of.' $'\n'
        return 1
    fi

    scryer-prolog \
        -g 'app__bsky__feed__getAuthorFeed("'"${author}"'", Prop).' \
        -g halt \
        ./src/app/bsky/feed/getAuthorFeed.pl
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

function infrastructure__lists__count() {
    configure

    scryer-prolog \
        -g 'count(Count), writeq(Count).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_lists.pl
}

function infrastructure__lists__query() {
    configure

    scryer-prolog \
        -g 'query(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_lists.pl
}

function infrastructure__lists__next_event_id() {
    configure

    scryer-prolog \
        -g 'next_event_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_lists.pl
}

function infrastructure__lists__next_id() {
    configure

    scryer-prolog \
        -g 'next_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_lists.pl
}

function infrastructure__list_items__count() {
    configure

    scryer-prolog \
        -g 'count(Count), writeq(Count)' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_list_items.pl
}

function infrastructure__list_items__query() {
    configure

    scryer-prolog \
        -g 'query(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_list_items.pl
}

function infrastructure__list_items__next_id() {
    configure

    scryer-prolog \
        -g 'next_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_list_items.pl
}

function infrastructure__publishers__count() {
    configure

    scryer-prolog \
        -g 'count(Count), writeq(Count)' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publishers.pl
}

function infrastructure__publishers__query() {
    configure

    scryer-prolog \
        -g 'query(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publishers.pl
}

function infrastructure__publishers__next_id() {
    configure

    scryer-prolog \
        -g 'next_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publishers.pl
}

function infrastructure__statuses__count() {
    configure

    scryer-prolog \
        -g 'count(Count), writeq(Count).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_statuses.pl
}

function infrastructure__statuses__query() {
    configure

    scryer-prolog \
        -g 'query(Result), write_term(Result, [double_quotes(true)]).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_statuses.pl
}

function infrastructure__statuses__next_id() {
    configure

    scryer-prolog \
        -g 'next_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_statuses.pl
}

function infrastructure__publications__count() {
    configure

    scryer-prolog \
        -g 'count(Count), writeq(Count).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publications.pl
}

function infrastructure__publications__query() {
    configure

    scryer-prolog \
        -g 'query(Result), write_term(Result, [double_quotes(true)]).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publications.pl
}

function infrastructure__publications__next_id() {
    configure

    scryer-prolog \
        -g 'next_id(Result), writeq(Result).' \
        -g 'halt.' \
        ./src/infrastructure/repository/repository_publications.pl
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
    \cat ./doc/api.json | \
    jq '.paths' | \
    jq 'keys[]' | \
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

function list_api_spec_keys() {
  local cmd
  cmd='echo ${1} > $(echo ${1}'
  # shellcheck disable=SC2089
  cmd="${cmd}"' | sed -E "s#/#./#g"'
  cmd="${cmd}"' | sed -E "s#(.*)#doc/endpoints/\1.key.json#g")'

  \cat ./doc/api.json | \
  jq '.paths | to_entries | .[] | .key' | \
  xargs -I{} sh -c "${cmd}" shell {}
}

function list_api_spec_values() {
  local value
  # shellcheck disable=SC2045
  for i in $(ls -1 ./doc/endpoints/*key.json);
  do
      value="$(echo $i | sed -E 's#key#value#')"
      jq ".paths | $(echo '."'"$(\cat $i)"'"')" ./doc/api.json \
      > "${value}";
  done
}

function test() {
  scryer-prolog ./src/types/string/formats/*_test.pl -g halt \
  | tee ./test.log
  # shellcheck disable=SC2046
  if [ $(grep -c '\[KO\]' ./test.log) -gt 0 ];
  then
    printf '%s.%s' 'Some tests failed' $'\n'
    exit 1
  else
    printf '%s.%s' 'All tests passed successfully' $'\n'
  fi
  rm ./test.log
}

set +Eeuo pipefail
