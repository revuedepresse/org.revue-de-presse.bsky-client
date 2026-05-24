:- module(sigsegv_http_repro, [run/0]).

:- use_module(library(assoc)).
:- use_module(library(format)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(serialization/json)).

:- use_module('../../src/stream', [read_stream/2]).
:- use_module('../../src/serialization', [pairs_to_assoc/2]).
:- use_module('../../src/domain/events/app/bsky/feed/event_getAuthorFeed', [
    onGetAuthorFeed/4,
    insert_record_args/9
]).

/*
HTTP-backed variant of sigsegv_minimal_repro.pl.

The minimal repro (read_term from disk) and the json_chars repro
(parse a regenerated JSON file) both SURVIVE on the patched VM with
the failing post. The live worker on the same patched VM hitting
public.api.bsky.app crashes 100%. The remaining variable is the
http_open + read_stream pipeline.

This reproducer drives http_open against a LOCAL server
(tests/pg/serve_feed_fixture.py) so the same call shape as
production runs without any Bluesky auth or remote dependency. If
this crashes, we have a fully self-contained reproducer suitable
for an upstream scryer issue.

Inputs (env)
------------
  FEED_URL     URL to GET (default http://127.0.0.1:8080/feed.json)
  POST_INDEX   0-based post index (default 0)
  DATABASE_*   same set as fun.sh's configure
*/

env_or_die(Name) :-
    (   getenv(Name, _) -> true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

env_chars_default(Name, Default, Value) :-
    (   getenv(Name, V) -> Value = V ; Value = Default ).

default_url("http://127.0.0.1:8080/feed.json").

load_feed_posts_via_http(Posts) :-
    default_url(DefaultUrl),
    env_chars_default("FEED_URL", DefaultUrl, URL),
    % HTTP_OPTS env knob: 'prod' = match production option set
    % (request_headers + status_code + headers); 'minimal' = empty
    % options list. Bisect dimension to determine whether the bug
    % needs the option allocations or just the http_open call itself.
    env_chars_default("HTTP_OPTS", "prod", OptsMode),
    (   OptsMode == "prod"
    ->  RequestHeaders = [
            'User-Agent'('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36')
        ],
        Options = [
            method(get),
            status_code(StatusCode),
            request_headers(RequestHeaders),
            headers(ResponseHeaders)
        ],
        format("[..] http_open ~s (HTTP_OPTS=prod)~n", [URL])
    ;   Options = [],
        format("[..] http_open ~s (HTTP_OPTS=minimal, empty options)~n", [URL])
    ),
    http_open(URL, Stream, Options),
    format("[..] stream opened~n", []),
    read_stream(Stream, BodyChars),
    length(BodyChars, BodyLen),
    format("[..] body chars=~w~n", [BodyLen]),
    BodyChars = [A,B,C,D,E|_],
    format("[..] first 5 chars: ~q ~q ~q ~q ~q~n", [A,B,C,D,E]),
    format("[..] parsing via phrase(json_chars(pairs(_)), Chars)~n", []),
    (   phrase(json_chars(pairs(ResponsePairs)), BodyChars)
    ->  format("[..] JSON-DCG parse succeeded~n", [])
    ;   format("[KO] JSON-DCG parse FAILED~n", []), halt(4)
    ),
    pairs_to_assoc(ResponsePairs, FeedAssoc),
    get_assoc(feed, FeedAssoc, Posts).

post_at_index(Posts, IndexChars, Index, Post) :-
    number_chars(Index, IndexChars),
    nth0(Index, Posts, Post).

run :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),
    env_chars_default("POST_INDEX", "0", IndexChars),

    load_feed_posts_via_http(Posts),
    length(Posts, Total),
    format("[..] posts=~w post_index=~s~n", [Total, IndexChars]),
    post_at_index(Posts, IndexChars, Index, Post),
    OneBased is Index + 1,
    format("[..] calling onGetAuthorFeed(none, ~w, Post, ~w) -- full production chain~n",
           [Total, OneBased]),
    format("[..] this exercises: exists_by_uri_t + likes/reposts fetch + status insert + publication insert~n", []),
    catch(
        ( onGetAuthorFeed(none, Total, Post, OneBased),
          format("[OK] onGetAuthorFeed survived for post_index=~w~n", [Index]),
          halt(0)
        ),
        Err,
        ( format("[KO] onGetAuthorFeed threw: ~q~n", [Err]),
          halt(1) )
    ).

:- initialization(run).
