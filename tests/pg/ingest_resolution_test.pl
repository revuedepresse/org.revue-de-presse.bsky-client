:- module(ingest_resolution_test, [run_test/0]).

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
Proof that a shortened link is resolved by the worker itself, at
ingestion, with nothing in the ledger beforehand and no browser in the
picture: a post carrying a live redirector is collected, and the row that
comes back carries the article's address.

This one reaches the network — a single GET to the redirector in the post
— so it is kept out of the default suite and run on purpose:

  make test-ingest-resolution

If the service is unreachable the link stays short and the ledger says
`pending`, which the test reports rather than failing on: that is the
designed behaviour, not a regression.
*/

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

live_short("https://l.humanite.fr/AXQ").
expected_host("www.humanite.fr").
probe_uri("at://probe-ingest-resolution-0").
probe_cursor("2025-06-15T23:59:59.000Z").

cleanup(Conn) :-
    pg_query_simple(Conn, "DELETE FROM status_popularity WHERE publication_id LIKE 'at://probe-ingest-resolution-%'", _),
    pg_query_simple(Conn, "DELETE FROM publication       WHERE document_id   LIKE 'at://probe-ingest-resolution-%'", _),
    pg_query_simple(Conn, "DELETE FROM weaving_status    WHERE ust_status_id LIKE 'at://probe-ingest-resolution-%'", _),
    pg_query_simple(Conn, "DELETE FROM publication_link  WHERE short_url = 'https://l.humanite.fr/AXQ'", _).

make_post(URI, Text, Post) :-
    Post = pairs([
        string("post")-pairs([
            string("uri")-string(URI),
            string("author")-pairs([
                string("did")-string("did:plc:probe"),
                string("handle")-string("probe.bsky.social"),
                string("displayName")-string("Probe"),
                string("avatar")-string("https://probe.example/x.png")
            ]),
            string("likeCount")-number(7),
            string("repostCount")-number(3),
            string("record")-pairs([
                string("text")-string(Text),
                string("createdAt")-string("2025-06-15T13:00:00Z")
            ])
        ])
    ]).

single_value(Conn, SQL, Param, Value) :-
    pg_query(Conn, SQL, [Param], Reply),
    Reply = data([[Value|_]|_]).

contains(Needle, Haystack) :-
    append(_, Suffix, Haystack),
    append(Needle, _, Suffix),
    !.

check(Name, Goal) :-
    (   call(Goal)
    ->  format("[ok] ~s~n", [Name])
    ;   format("[KO] ~s~n", [Name]), fail ).

run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    live_short(Short),
    expected_host(Host),
    probe_uri(URI),
    probe_cursor(Cursor),

    open_pg_connection(C0),
    cleanup(C0),

    append(["Notre enquête 👉 ", Short], Text),
    make_post(URI, Text, Post),
    onGetAuthorFeed(pg_session(C0, C1), Cursor, 1, Post, 1),

    single_value(C1,
        "SELECT convert_from(decode(text, 'base64'), 'UTF8') FROM publication WHERE document_id = $1",
        URI, Stored),
    single_value(C1, "SELECT status FROM publication_link WHERE short_url = $1", Short, Status),
    format("[..] stored: ~s~n", [Stored]),
    format("[..] ledger: ~s~n", [Status]),
    close_pg_connection(C1),

    (   Status == "resolved"
    ->  (   check("the worker resolved the link at ingestion", Status == "resolved"),
            check("the stored text carries the article's address", contains(Host, Stored)),
            check("and no longer the redirector's token", \+ contains(Short, Stored))
        ->  format("[OK] ingest_resolution_test passed~n", []), halt(0)
        ;   format("[KO] ingest_resolution_test failed~n", []), halt(1) )
    ;   format("[--] the service did not answer (~s); the link stayed short, as designed~n", [Status]),
        halt(0) ).
