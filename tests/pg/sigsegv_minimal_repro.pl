:- module(sigsegv_minimal_repro, [run/0]).

:- use_module(library(assoc)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/serialization', [pairs_to_assoc/2]).
:- use_module('../../src/domain/events/app/bsky/feed/event_getAuthorFeed', [
    insert_record_args/9
]).
:- use_module('../../src/infrastructure/repository/repository_status', [
    insert/3
]).

/*
Minimal reproducer for the patched-VM SIGSEGV in unify_constant.

Replays exactly what onGetAuthorFeed/2 (event_getAuthorFeed.pl)
does on a single captured post:

    insert_record_args(Post, DN, Handle, Text, Avatar, Payload, URI, CreatedAt, _).
    repository_status:insert(
        row(DN, Handle, Text, Avatar, Payload, URI, CreatedAt),
        Result, RecordId
    ).

Production crash pattern (seen on every lemonde.fr run on the
patched VM v0.10.0-186):

    [no_records_found_by_uri,"at://.../<URI>"]
    [likes_reposts,likes(N)-reposts(M)]
    fun.sh: ... Segmentation fault (core dumped)

The crash happens between `[likes_reposts]` and the absent
`[status_inserted]` log line -- which is exactly inside
repository_status:insert/3 -> pg_query/3 -> postgresql-prolog wire
client. This file isolates that call by loading the same captured
JSON-DCG pair tree the production worker had in flight, walking to
post[POST_INDEX] (default 1, matching the production log
"processing_post_at_index,1/15"), and firing the insert directly.

Inputs (env)
------------
  FEED_PAIRS_FIXTURE  path to a captured last-feed-pairs.pl
                      (default: var/tmp/segv-investigation/
                       crash-20260524T170851Z-pid3985412/
                       last-feed-pairs.pl)
  POST_INDEX          0-based index in the feed (default 1)
  DATABASE_*          same set as fun.sh's configure

Exit codes
----------
   0  insert returned cleanly (bug did not reproduce)
   1  threw an exception (not a SIGSEGV)
   2  missing required env
 139  SIGSEGV (kernel-level signal -- can't be caught from Prolog)
*/

env_or_die(Name) :-
    (   getenv(Name, _) -> true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

env_chars_default(Name, Default, Value) :-
    (   getenv(Name, V) -> Value = V ; Value = Default ).

default_fixture(
    "var/tmp/segv-investigation/crash-20260524T170851Z-pid3985412/last-feed-pairs.pl"
).

load_feed_posts(Posts) :-
    default_fixture(DefaultPath),
    env_chars_default("FEED_PAIRS_FIXTURE", DefaultPath, Path),
    format("[..] fixture=~s~n", [Path]),
    open(Path, read, Stream),
    catch(
        read_term(Stream, Term, []),
        E,
        (close(Stream), throw(E))
    ),
    close(Stream),
    Term = last_feed_pairs(ResponsePairs),
    pairs_to_assoc(ResponsePairs, FeedAssoc),
    get_assoc(feed, FeedAssoc, Posts).

post_at_index(Posts, IndexChars, Index, Post) :-
    number_chars(Index, IndexChars),
    nth0(Index, Posts, Post).

run :-
    % postgresql-prolog's connect/6 is consulted lazily inside
    % repository_status:insert/3 -- but it does need the env.
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    env_chars_default("POST_INDEX", "1", IndexChars),
    load_feed_posts(Posts),
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
    format("[..] firing repository_status:insert -- this is where prod segfaults~n", []),

    catch(
        ( repository_status:insert(
              row(DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt),
              InsertionResult,
              RecordId
          ),
          format("[OK] survived: insertion_result=~q record_id=~q post_index=~w~n",
                 [InsertionResult, RecordId, Index]),
          halt(0)
        ),
        InsertErr,
        ( format("[KO] repository_status:insert threw: ~q~n", [InsertErr]),
          halt(1) )
    ).

:- initialization(run).
