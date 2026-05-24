:- module(sigsegv_chainbisect_repro, [run/0]).

:- use_module(library(assoc)).
:- use_module(library(format)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(serialization/json)).

:- use_module('../../src/stream', [read_stream/2]).
:- use_module('../../src/serialization', [pairs_to_assoc/2]).
:- use_module('../../src/domain/events/app/bsky/feed/event_getAuthorFeed', [
    insert_record_args/9
]).
:- use_module('../../src/infrastructure/repository/repository_status', [
    exists_by_uri_t/3
]).

/*
Call-chain bisector. Mirrors sigsegv_http_repro.pl up through the
http_open + json_chars + insert_record_args path, then runs a
subset of the production pg_query chain controlled by CHAIN_LEVEL:

  0  insert_record_args only        -- no pg_query
  1  + exists_by_uri_t              -- + Q0: SELECT count(*) ust_hash
  2  + repository_status:insert     -- + Q1: INSERT status RETURNING ust_id
  3  + repository_popularity:insert -- + Q2: INSERT popularity
  4  + repository_publication:insert -- + Q3: INSERT publication (== full prod chain)

Together with JSON_PATH (which capture's reconstructed JSON to
serve) and POST_INDEX, this lets us identify the minimum subset
that trips the SIGSEGV. The level at which a survivor flips to a
crash IS the suspect pg_query.

The reproducer always uses HTTP_OPTS=prod (request_headers +
status_code + headers) since we already established the bug
needs the prod option set.
*/

env_or_die(Name) :-
    (   getenv(Name, _) -> true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

env_chars_default(Name, Default, Value) :-
    (   getenv(Name, V) -> Value = V ; Value = Default ).

env_number_default(Name, Default, Number) :-
    (   getenv(Name, Chars) -> number_chars(Number, Chars) ; Number = Default ).

default_url("http://127.0.0.1:8080/feed.json").

load_feed_posts_via_http(Posts) :-
    default_url(DefaultUrl),
    env_chars_default("FEED_URL", DefaultUrl, URL),
    RequestHeaders = [
        'User-Agent'('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36')
    ],
    Options = [
        method(get),
        status_code(_StatusCode),
        request_headers(RequestHeaders),
        headers(_ResponseHeaders)
    ],
    format("[..] http_open ~s~n", [URL]),
    http_open(URL, Stream, Options),
    read_stream(Stream, BodyChars),
    length(BodyChars, BodyLen),
    format("[..] body chars=~w~n", [BodyLen]),
    phrase(json_chars(pairs(ResponsePairs)), BodyChars),
    pairs_to_assoc(ResponsePairs, FeedAssoc),
    get_assoc(feed, FeedAssoc, Posts).

run :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),
    env_number_default("POST_INDEX", 0, Index),
    env_number_default("CHAIN_LEVEL", 4, Level),
    format("[..] POST_INDEX=~w CHAIN_LEVEL=~w~n", [Index, Level]),

    load_feed_posts_via_http(Posts),
    nth0(Index, Posts, Post),

    insert_record_args(
        Post,
        DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
        likes(LikeCount)-reposts(RepostCount)
    ),
    format("[..] insert_record_args ok handle=~s uri=~s~n", [Handle, URI]),

    chain_step(Level, 0,
        format("[..] CHAIN_LEVEL=0 -- stop after insert_record_args~n", [])),

    chain_step(Level, 1,
        ( format("[..] Q0: exists_by_uri_t(~s, ~s, T)~n", [Handle, URI]),
          exists_by_uri_t(Handle, URI, T0),
          format("[..] Q0 returned T=~q (continuing regardless)~n", [T0])
        )),

    chain_step(Level, 2,
        ( format("[..] Q1: repository_status:insert/3~n", []),
          repository_status:insert(
              row(DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt),
              StatusResult, RecordId
          ),
          format("[..] Q1 returned ~q record_id=~q~n", [StatusResult, RecordId])
        )),

    chain_step(Level, 3,
        ( format("[..] Q2: repository_popularity:insert_without_unicity_check/2~n", []),
          repository_popularity:insert_without_unicity_check(
              row(RecordId, URI, LikeCount, RepostCount),
              PopResult
          ),
          format("[..] Q2 returned ~q~n", [PopResult])
        )),

    chain_step(Level, 4,
        ( format("[..] Q3: repository_publication:insert/2~n", []),
          repository_publication:insert(
              row(DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt, RecordId),
              PubResult
          ),
          format("[..] Q3 returned ~q~n", [PubResult])
        )),

    format("[OK] survived CHAIN_LEVEL=~w on post_index=~w~n", [Level, Index]),
    halt(0).

chain_step(Level, Required, Goal) :-
    (   Level >= Required
    ->  call(Goal)
    ;   true
    ).

:- initialization(run).
