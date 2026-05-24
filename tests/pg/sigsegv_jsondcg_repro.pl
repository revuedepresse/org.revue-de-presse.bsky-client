:- module(sigsegv_jsondcg_repro, [run/0]).

:- use_module(library(assoc)).
:- use_module(library(format)).
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
JSON-DCG variant of sigsegv_minimal_repro.pl.

Background
----------
sigsegv_minimal_repro.pl + the failing-post capture loaded via
read_term/2 + insert/3 does NOT crash. The same Prolog data shape
through the same insert call survives cleanly. That implicates the
arena allocation pattern of the JSON-DCG parse (the production code
path) rather than the post data itself.

This file exercises the JSON-DCG path against a reconstructed feed
body. reconstruct_feed_json.pl writes the actual JSON text out
from a captured last-feed-body.txt; here we read that JSON back
as chars and run `phrase(json_chars(pairs(_)), Chars)` -- the same
predicate getAuthorFeed.pl:117 calls on a live HTTP stream. Then
the same insert path as the minimal repro.

If this reproduces the SIGSEGV, the bug is reachable purely via:
    json_chars DCG parse of N kilobytes of JSON
  + pairs_to_assoc walk
  + repository_status:insert/3 (which calls pg_query)
with no http_open, no concurrent workers, no cpulimit. That makes
the upstream-postable reproducer one short step away.

If it does NOT reproduce, the live http_open stream is the missing
ingredient and we need to wire a local HTTP server too.

Inputs (env)
------------
  FEED_JSON_FIXTURE   path to a reconstructed JSON body
                      (default: var/tmp/segv-investigation/
                       crash-20260524T170851Z-pid3985412/
                       feed-body.json)
  POST_INDEX          0-based post index (default 0)
  DATABASE_*          same set as fun.sh's configure
*/

env_or_die(Name) :-
    (   getenv(Name, _) -> true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

env_chars_default(Name, Default, Value) :-
    (   getenv(Name, V) -> Value = V ; Value = Default ).

default_fixture(
    "var/tmp/segv-investigation/crash-20260524T170851Z-pid3985412/feed-body.json"
).

load_feed_posts_via_jsondcg(Posts) :-
    default_fixture(DefaultPath),
    env_chars_default("FEED_JSON_FIXTURE", DefaultPath, Path),
    format("[..] fixture=~s~n", [Path]),
    open(Path, read, Stream),
    read_stream(Stream, BodyChars),
    length(BodyChars, BodyLen),
    format("[..] body chars=~w~n", [BodyLen]),
    BodyChars = [A,B,C,D,E|_],
    format("[..] first 5 chars: ~q ~q ~q ~q ~q~n", [A,B,C,D,E]),
    format("[..] parsing via phrase(json_chars(pairs(_)), Chars) -- the production path~n", []),
    (   phrase(json_chars(pairs(ResponsePairs)), BodyChars)
    ->  format("[..] JSON-DCG parse succeeded~n", [])
    ;   format("[KO] phrase(json_chars(pairs(_)), Chars) FAILED on ~w chars~n", [BodyLen]),
        halt(4)
    ),
    (   pairs_to_assoc(ResponsePairs, FeedAssoc)
    ->  format("[..] pairs_to_assoc succeeded~n", [])
    ;   format("[KO] pairs_to_assoc FAILED~n", []),
        halt(5)
    ),
    (   get_assoc(feed, FeedAssoc, Posts)
    ->  format("[..] get_assoc(feed, ...) succeeded~n", [])
    ;   format("[KO] get_assoc(feed, ...) FAILED~n", []),
        halt(6)
    ).

post_at_index(Posts, IndexChars, Index, Post) :-
    number_chars(Index, IndexChars),
    nth0(Index, Posts, Post).

run :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    env_chars_default("POST_INDEX", "0", IndexChars),
    load_feed_posts_via_jsondcg(Posts),
    length(Posts, Total),
    format("[..] posts=~w post_index=~s~n", [Total, IndexChars]),

    post_at_index(Posts, IndexChars, Index, Post),
    OneBased is Index + 1,
    format("[..] calling onGetAuthorFeed(none, ~w, Post, ~w) -- full prod chain via file-loaded JSON~n",
           [Total, OneBased]),
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
