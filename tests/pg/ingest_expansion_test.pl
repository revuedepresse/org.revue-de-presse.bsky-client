:- module(ingest_expansion_test, [run_test/0]).

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
:- use_module('../../src/infrastructure/repository/repository_publication_link', [
    record_expansion/4
]).

/*
What a collected post actually carries, driven through the real handler
rather than through the expansion predicate on its own.

  1. a post whose link the ledger knows is stored with the address that
     link leads to, in `publication` and in `weaving_status` alike;
  2. the same post seen an hour later takes the other branch of
     handle_existence/4 — the one that only re-measures popularity — and
     must not reintroduce the shortened form;
  3. a link nobody has resolved leaves the post exactly as written and
     lands on the resolver's worklist.

Run with:
  make test-ingest-expansion
*/

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

known_short("https://l.probe-ingest.test/Kx7").
known_expanded("https://www.mediapart.fr/journal/france/050325/enquete-sur-le-maintien-de-l-ordre").
unknown_short("https://l.probe-ingest.test/Zz2").

known_uri("at://probe-ingest-expansion-0").
unknown_uri("at://probe-ingest-expansion-1").
probe_cursor("2025-06-15T23:59:59.000Z").

cleanup(Conn) :-
    pg_query_simple(Conn, "DELETE FROM status_popularity WHERE publication_id LIKE 'at://probe-ingest-expansion-%'", _),
    pg_query_simple(Conn, "DELETE FROM publication       WHERE document_id   LIKE 'at://probe-ingest-expansion-%'", _),
    pg_query_simple(Conn, "DELETE FROM weaving_status    WHERE ust_status_id LIKE 'at://probe-ingest-expansion-%'", _),
    pg_query_simple(Conn, "DELETE FROM publication_link  WHERE short_url     LIKE 'https://l.probe-ingest.test/%'", _).

make_post(URI, Text, LikeCount, RepostCount, Post) :-
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
                string("text")-string(Text),
                string("createdAt")-string("2025-06-15T13:00:00Z")
            ])
        ])
    ]).

feed_post(Conn0, Post, Conn, Outcome) :-
    probe_cursor(Cursor),
    catch(
        ( onGetAuthorFeed(pg_session(Conn0, Conn), Cursor, 1, Post, 1),
          Outcome = returned ),
        E,
        ( Outcome = thrown(E), Conn = Conn0 )
    ).

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

    known_short(Short),
    known_expanded(Expanded),
    unknown_short(Unknown),
    known_uri(KnownURI),
    unknown_uri(UnknownURI),

    open_pg_connection(C0),
    cleanup(C0),

    % The resolver has answered for this one.
    record_expansion(pg_session(C0, C1), short_url(Short), expanded(Expanded), _),

    append(["Notre enquête 👉 ", Short], KnownText),
    make_post(KnownURI, KnownText, 5, 2, FirstSighting),
    feed_post(C1, FirstSighting, C2, FirstOutcome),

    % An hour later, same post, more reposts: the re-measure branch.
    make_post(KnownURI, KnownText, 40, 17, SecondSighting),
    feed_post(C2, SecondSighting, C3, SecondOutcome),

    append(["À lire 👉 ", Unknown], UnknownText),
    make_post(UnknownURI, UnknownText, 3, 1, Unresolved),
    feed_post(C3, Unresolved, C4, UnknownOutcome),

    % Text crosses the write boundary base64-encoded (client.pl's
    % encode_field_value/2), so the assertions read the decoded bytes —
    % what the API hands to the rankings and the feeds.
    single_value(C4,
        "SELECT convert_from(decode(text, 'base64'), 'UTF8') FROM publication WHERE document_id = $1",
        KnownURI, StoredPublication),
    single_value(C4,
        "SELECT convert_from(decode(ust_text, 'base64'), 'UTF8') FROM weaving_status WHERE ust_status_id = $1",
        KnownURI, StoredStatus),
    single_value(C4,
        "SELECT convert_from(decode(text, 'base64'), 'UTF8') FROM publication WHERE document_id = $1",
        UnknownURI, StoredUnresolved),
    single_value(C4, "SELECT status FROM publication_link WHERE short_url = $1", Unknown, PendingStatus),

    format("[..] publication: ~s~n", [StoredPublication]),
    format("[..] status:      ~s~n", [StoredStatus]),
    close_pg_connection(C4),

    (   check("the post is collected without error", FirstOutcome == returned),
        check("the second sighting completes too", SecondOutcome == returned),
        check("the post with an unresolved link is collected", UnknownOutcome == returned),
        check("publication.text carries the address the link leads to",
              contains(Expanded, StoredPublication)),
        check("and no longer the shortened form",
              \+ contains(Short, StoredPublication)),
        check("weaving_status.ust_text says the same thing",
              contains(Expanded, StoredStatus)),
        check("a link nobody resolved is left exactly as the post wrote it",
              contains(Unknown, StoredUnresolved)),
        check("and is on the resolver's worklist", PendingStatus == "pending")
    ->  format("[OK] ingest_expansion_test passed~n", []), halt(0)
    ;   format("[KO] ingest_expansion_test failed~n", []), halt(1) ).
