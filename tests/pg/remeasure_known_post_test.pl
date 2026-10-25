:- module(remeasure_known_post_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [
    open_pg_connection/1,
    close_pg_connection/1,
    pg_query/4,
    pg_query_simple/3
]).
:- use_module('../../src/domain/events/app/bsky/feed/event_getAuthorFeed', [
    onGetAuthorFeed/5
]).

/*
Regression: a post the worker already stored must still get its
popularity re-measured each time the feed returns it. Every hourly
run appends a status_popularity row for each post still within the
2-day pagination window, which is what the daily top 10 ranks on
(max reposts/likes measured on the publication day).

The test feeds the same post twice through onGetAuthorFeed/5, with
different counts, and expects:

  - 1 weaving_status row and 1 publication row (no duplicates),
  - 2 status_popularity rows, carrying the first then the second
    counts.
*/

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

probe_uri("at://probe-remeasure-0").
probe_cursor("2025-06-15T23:59:59.000Z").

cleanup_probe_rows(Conn) :-
    pg_query_simple(Conn, "DELETE FROM status_popularity WHERE publication_id LIKE 'at://probe-remeasure-%'", _),
    pg_query_simple(Conn, "DELETE FROM publication       WHERE document_id   LIKE 'at://probe-remeasure-%'", _),
    pg_query_simple(Conn, "DELETE FROM weaving_status    WHERE ust_status_id LIKE 'at://probe-remeasure-%'", _).

%% Synthetic feed post in the wrapped-pairs shape insert_record_args/9
%% expects after the JSON DCG decode.
make_post(URI, LikeCount, RepostCount, Post) :-
    Post = pairs([
        string("post")-pairs([
            string("uri")-string(URI),
            string("author")-pairs([
                string("did")-string("did:plc:probe"),
                string("handle")-string("probe.bsky.social"),
                string("displayName")-string("Probe"),
                string("avatar")-string("https://probe.example/x.png")
            ]),
            string("likeCount")-number(LikeCount),
            string("repostCount")-number(RepostCount),
            string("record")-pairs([
                string("text")-string("remeasured post"),
                string("createdAt")-string("2025-06-15T13:00:00Z")
            ])
        ])
    ]).

%% feed_post(+Conn0, +Post, -Conn, -Outcome).
%
% Outcome is `returned` when onGetAuthorFeed/5 completes, or
% `thrown(E)` when it raises (the fold in getAuthorFeed.pl catches
% already_indexed_post the same way).
feed_post(Conn0, Post, Conn, Outcome) :-
    probe_cursor(Cursor),
    catch(
        ( onGetAuthorFeed(pg_session(Conn0, Conn), Cursor, 1, Post, 1),
          Outcome = returned ),
        E,
        ( Outcome = thrown(E), Conn = Conn0 )
    ).

count_rows(Conn, SQL, URI, N) :-
    pg_query(Conn, SQL, [URI], Reply),
    extract_count(Reply, N).

extract_count(data([[NChars|_]|_]), N) :- number_chars(N, NChars).

measurements(Conn, URI, Measurements) :-
    pg_query(Conn,
        "SELECT total_favorites::text, total_retweets::text FROM status_popularity WHERE publication_id = $1 ORDER BY checked_at, id",
        [URI], Reply),
    Reply = data(Rows),
    maplist(row_to_measurement, Rows, Measurements).

row_to_measurement([LikesChars, RepostsChars|_], likes(Likes)-reposts(Reposts)) :-
    number_chars(Likes, LikesChars),
    number_chars(Reposts, RepostsChars).

check(Name, Goal) :-
    (   call(Goal)
    ->  format("[ok] ~s~n", [Name])
    ;   format("[KO] ~s~n", [Name]), fail ).

run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),
    probe_uri(URI),

    open_pg_connection(C0),
    cleanup_probe_rows(C0),

    make_post(URI, 5, 2, FirstSighting),
    feed_post(C0, FirstSighting, C1, FirstOutcome),
    format("[..] first sighting: ~q~n", [FirstOutcome]),

    make_post(URI, 40, 17, SecondSighting),
    feed_post(C1, SecondSighting, C2, SecondOutcome),
    format("[..] second sighting: ~q~n", [SecondOutcome]),

    count_rows(C2, "SELECT count(*)::text FROM weaving_status WHERE ust_status_id = $1", URI, Statuses),
    count_rows(C2, "SELECT count(*)::text FROM publication WHERE document_id = $1", URI, Publications),
    measurements(C2, URI, Measurements),
    format("[..] statuses=~w publications=~w measurements=~q~n", [Statuses, Publications, Measurements]),
    close_pg_connection(C2),

    (   check("first sighting is stored without error", FirstOutcome == returned),
        check("second sighting of a known post completes", SecondOutcome == returned),
        check("exactly one status row", Statuses =:= 1),
        check("exactly one publication row", Publications =:= 1),
        check("one measurement per sighting, in order",
              Measurements == [likes(5)-reposts(2), likes(40)-reposts(17)])
    ->  format("[OK] known posts are re-measured~n", []),
        halt(0)
    ;   format("[KO] known posts are not re-measured~n", []),
        halt(1)
    ).
