:- module(already_indexed_by_uri_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [pg_query_simple/2]).
:- use_module('../../src/infrastructure/repository/repository_status', [insert/3]).
:- use_module('../../src/domain/events/app/bsky/feed/event_getAuthorFeed', [
    onGetAuthorFeed/4
]).

/*
Regression: onGetAuthorFeed/2 must dedup by the post's own URI,
not by the page cursor.

The buggy lookup at event_getAuthorFeed.pl:76 was

    by_indexed_at(indexed_at(Cursor)-handle(Handle), Rows)

where `Cursor` is the API's NextCursor (= the page's *oldest*
post's indexedAt). When any pre-existing weaving_status row for
the same handle has ust_created_at equal to that cursor (which
happens for every page whose oldest post was indexed in a
previous worker cycle), the very first post threw
already_indexed_post and the maplist halted -- so NEW posts on
that page never got inserted. Combined with the threaded-anchor
pagination cutoff (which terminates within ~2 days of page 1's
top post), the worker stopped reaching pages where the cursor
mismatch would have allowed inserts. Hence the May 23-24
throughput collapse.

Three cases drive the fix:

  1. Cursor *collides* with a baseline row's timestamp; the
     incoming post's URI is *new*. The dedup must NOT fire. This
     is the bug case -- the one the worker hit in production.

  2. Incoming post's URI exactly matches a baseline row. The
     dedup MUST fire (throw already_indexed_post). Proves the
     fix didn't disable per-post dedup.

  3. Cursor doesn't collide with anything; URI is new. The dedup
     must NOT fire and the row must land. Proves the happy path
     still works.
*/

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

baseline_handle("probe.bsky.social").
baseline_uri("at://probe-baseline-0").
baseline_cursor_ts("2025-06-15 12:00:00").
baseline_text("baseline text").

%% Cursor is the ISO-8601-with-T form of baseline_cursor_ts/1;
%% by_indexed_at/2 parses it to "2025-06-15 12:00:00", which
%% used to match the baseline row and fire the bug.
colliding_cursor("2025-06-15T12:00:00.123Z").

%% A cursor seconds away from any baseline row, used for case 3.
pristine_cursor("2025-06-15T23:59:59.000Z").

cleanup_probe_rows :-
    pg_query_simple("DELETE FROM status_popularity WHERE publication_id LIKE 'at://probe-%'", _),
    pg_query_simple("DELETE FROM publication       WHERE document_id   LIKE 'at://probe-%'", _),
    pg_query_simple("DELETE FROM weaving_status    WHERE ust_status_id LIKE 'at://probe-%'", _).

seed_baseline :-
    baseline_handle(H), baseline_uri(U), baseline_cursor_ts(Ts), baseline_text(Txt),
    repository_status:insert(
        row(_DN, H, Txt, "avatar.png", "{}", U, Ts),
        Result, UstId
    ),
    format("[..] seeded baseline -> ~w ust_id=~s~n", [Result, UstId]).

%% Construct a synthetic feed post in the wrapped-pairs shape that
%% insert_record_args/9 expects after the JSON DCG decode.
%% URI/text/createdAt are the per-test variables.
make_post(URI, CreatedAt, Text, Post) :-
    Post = pairs([
        string("post")-pairs([
            string("uri")-string(URI),
            string("author")-pairs([
                string("did")-string("did:plc:probe"),
                string("handle")-string("probe.bsky.social"),
                string("displayName")-string("Probe"),
                string("avatar")-string("https://probe.example/x.png")
            ]),
            string("likeCount")-number(5),
            string("repostCount")-number(2),
            string("record")-pairs([
                string("text")-string(Text),
                string("createdAt")-string(CreatedAt)
            ])
        ])
    ]).

uri_row_count(URI, N) :-
    append(["SELECT count(*)::int FROM weaving_status WHERE ust_status_id = '", URI, "'"], SQL),
    pg_query_simple(SQL, Reply),
    extract_count(Reply, N).

extract_count(data(_, [[NChars]]), N) :- number_chars(N, NChars).
extract_count(data([[NChars|_]|_]), N) :- number_chars(N, NChars).

%% --- Case 1: cursor collides, new URI -> must insert ---------------
case_cursor_collides_new_uri_inserts(Result) :-
    colliding_cursor(Cursor),
    make_post("at://probe-new-1", "2025-06-15T13:00:00Z", "new post one", Post),
    format("[..] case1 driving cursor=~s (collides) uri=at://probe-new-1 (new)~n", [Cursor]),
    catch(
        onGetAuthorFeed(Cursor, 1, Post, 1),
        Err,
        Caught = thrown(Err)
    ),
    uri_row_count("at://probe-new-1", N),
    (   var(Caught), N =:= 1
    ->  Result = ok,
        format("[ok] case1: new URI inserted, no false throw~n", [])
    ;   Caught = thrown(already_indexed_post(uri(_)))
    ->  Result = ko(case1_false_throw),
        format("[KO] case1: already_indexed_post fired on a NEW URI -- bug still present~n", [])
    ;   nonvar(Caught)
    ->  Result = ko(case1_unexpected(Caught)),
        format("[KO] case1: unexpected throw ~q~n", [Caught])
    ;   Result = ko(case1_missing_row(N)),
        format("[KO] case1: no throw but row count = ~w~n", [N])
    ).

%% --- Case 2: incoming URI matches baseline -> must throw ----------
case_existing_uri_throws(Result) :-
    pristine_cursor(Cursor),
    baseline_uri(URI),
    make_post(URI, "2025-06-15T12:00:00Z", "duplicate post", Post),
    format("[..] case2 driving uri=~s (already exists) cursor=~s~n", [URI, Cursor]),
    catch(
        onGetAuthorFeed(Cursor, 1, Post, 1),
        Err,
        Caught = thrown(Err)
    ),
    (   Caught = thrown(already_indexed_post(uri(_)))
    ->  Result = ok,
        format("[ok] case2: per-URI dedup correctly throws on duplicate~n", [])
    ;   var(Caught)
    ->  Result = ko(case2_missing_throw),
        format("[KO] case2: duplicate URI didn't throw -- dedup disabled~n", [])
    ;   Result = ko(case2_unexpected(Caught)),
        format("[KO] case2: unexpected throw ~q~n", [Caught])
    ).

%% --- Case 3: cursor doesn't collide, URI is new -> must insert ----
case_happy_path_inserts(Result) :-
    pristine_cursor(Cursor),
    make_post("at://probe-new-3", "2025-06-15T14:00:00Z", "happy path", Post),
    format("[..] case3 driving cursor=~s (no collision) uri=at://probe-new-3 (new)~n", [Cursor]),
    catch(
        onGetAuthorFeed(Cursor, 1, Post, 1),
        Err,
        Caught = thrown(Err)
    ),
    uri_row_count("at://probe-new-3", N),
    (   var(Caught), N =:= 1
    ->  Result = ok,
        format("[ok] case3: happy path inserted cleanly~n", [])
    ;   nonvar(Caught)
    ->  Result = ko(case3_unexpected(Caught)),
        format("[KO] case3: unexpected throw ~q~n", [Caught])
    ;   Result = ko(case3_missing_row(N)),
        format("[KO] case3: no throw but row count = ~w~n", [N])
    ).

run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    cleanup_probe_rows,
    seed_baseline,

    case_cursor_collides_new_uri_inserts(R1),
    case_existing_uri_throws(R2),
    case_happy_path_inserts(R3),

    (   R1 == ok, R2 == ok, R3 == ok
    ->  format("[OK] all 3 dedup-by-URI cases passed~n", []),
        halt(0)
    ;   format("[KO] one or more cases failed: r1=~q r2=~q r3=~q~n", [R1, R2, R3]),
        halt(1)
    ).
