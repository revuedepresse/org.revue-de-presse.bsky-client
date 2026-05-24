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
    insert_record_args/9
]).
:- use_module('../../src/infrastructure/repository/repository_status', [
    insert/3
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
    % Match the production option set exactly: request_headers
    % with a User-Agent (Authorization is omitted -- localhost
    % server doesn't check, but the option-shape is what we are
    % testing), status_code, headers(ResponseHeaders).
    RequestHeaders = [
        'User-Agent'('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36')
    ],
    Options = [
        method('GET'),
        status_code(StatusCode),
        request_headers(RequestHeaders),
        headers(ResponseHeaders)
    ],
    format("[..] http_open ~s (with prod options)~n", [URL]),
    http_open(URL, Stream, Options),
    format("[..] stream opened status_code=~w headers=~q~n", [StatusCode, ResponseHeaders]),
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

    catch(
        insert_record_args(
            Post,
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            likes(LikeCount)-reposts(RepostCount)
        ),
        ExtractErr,
        ( format("[KO] insert_record_args threw: ~q~n", [ExtractErr]),
          halt(1) )
    ),

    format("[..] handle=~s uri=~s~n", [Handle, URI]),
    format("[..] likes=~w reposts=~w~n", [LikeCount, RepostCount]),
    format("[..] firing repository_status:insert -- prod crash site~n", []),

    catch(
        ( repository_status:insert(
              row(DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt),
              InsertionResult, RecordId
          ),
          format("[OK] survived: ~q record_id=~q~n", [InsertionResult, RecordId]),
          halt(0)
        ),
        InsertErr,
        ( format("[KO] repository_status:insert threw: ~q~n", [InsertErr]),
          halt(1) )
    ).

:- initialization(run).
