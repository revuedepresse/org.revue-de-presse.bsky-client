:- module(list_member_profile_test, [run_test/0]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(serialization/json)).

:- use_module('../../src/infrastructure/pg/connection', [
    close_pg_connection/1,
    open_pg_connection/1,
    pg_query/4,
    pg_query_simple/3
]).
:- use_module('../../src/app/bsky/graph/getList', [import_member/5]).
:- use_module('../../src/domain/events/app/bsky/graph/event_getList', [onGetList/3]).
:- use_module('../../src/domain/events/app/bsky/actor/event_getProfile', [onGetProfile/2]).
:- use_module('../../src/serialization', [
    char_code_at/2,
    pairs_to_assoc/2,
    to_json_chars/2,
    wrapped_pairs_to_assoc/2
]).

/*
Re-running an interrupted list import must not request
app.bsky.actor.getProfile again for members already in publishers_list
for the list: import_member/5 must look the membership up first and
skip the request when it is recorded. The fetchers are injected, so
the test counts calls without any network; member feeds are off by
default, so the feed fetcher must not run either.
*/

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

list_uri("at://did:plc:member-profile-probe/app.bsky.graph.list/probe").

probe_member("did:plc:member-profile-fresh", "member-profile-fresh.bsky.social").
probe_member("did:plc:member-profile-recorded", "member-profile-recorded.bsky.social").

:- dynamic(fetched/2).

profile_pairs([string("followersCount")-number("10"), string("followsCount")-number("5")]).

recording_profile(Actor, Payload) :-
    assertz(fetched(profile, Actor)),
    profile_pairs(Payload).
recording_feed(Actor, fetched) :-
    assertz(fetched(feed, Actor)).

fetchers(fetchers(list_member_profile_test:recording_profile, list_member_profile_test:recording_feed)).

fetches(Kind, DID, Count) :-
    findall(x, fetched(Kind, DID), Xs),
    length(Xs, Count).

%% item_assoc(+DID, +Handle, -Assoc)
%
% A getList item, wrapped the way getList.pl's process_items/3 does.
item_assoc(DID, Handle, Assoc) :-
    list_uri(ListURI),
    append([
        "{\"uri\":\"", ListURI, "-item\",",
        "\"subject\":{\"did\":\"", DID, "\",\"handle\":\"", Handle, "\"}}"
    ], ItemChars),
    phrase(json_chars(pairs(ItemPairs)), ItemChars),
    wrapped_pairs_to_assoc(pairs(ItemPairs), Assoc).

list_payload(Payload) :-
    list_uri(ListURI),
    append(["{\"list\":{\"uri\":\"", ListURI, "\",\"name\":\"member-profile-probe\"},\"items\":[]}"], BodyChars),
    to_json_chars(BodyChars, JSONChars),
    write_term_to_chars(JSONChars, [quoted(false),double_quotes(true)], Chars),
    chars_utf8bytes(Chars, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, Utf8BytesPayload),
    chars_base64(Utf8BytesPayload, Payload, []).

memberships(Conn, DID, Count) :-
    list_uri(ListURI),
    pg_query(Conn, "SELECT count(*)::text FROM publishers_list WHERE name = $1 AND screen_name = $2",
        [ListURI, DID], Reply),
    (   Reply = data([[Count]])
    ->  true
    ;   format("[KO] membership count returned ~w~n", [Reply]), halt(1) ).

expect(Label, Expected, Actual, Failures0, Failures) :-
    (   Expected == Actual
    ->  format("[OK] ~s~n", [Label]), Failures = Failures0
    ;   format("[KO] ~s~nexpected: ~w~nactual: ~w~n", [Label, Expected, Actual]),
        Failures is Failures0 + 1
    ).

%% delete_probe_rows(+Conn)
%
% The probe list URI is a constant, so it is inlined in simple queries.
delete_probe_rows(Conn) :-
    list_uri(ListURI),
    append(["DELETE FROM publishers_list_collected_event WHERE list_name = '", ListURI, "'"], DeleteEvents),
    append(["DELETE FROM publishers_list WHERE name = '", ListURI, "'"], DeleteMemberships),
    pg_query_simple(Conn, DeleteEvents, _),
    pg_query_simple(Conn, DeleteMemberships, _).

run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME","TEMP_DIR"]),
    unsetenv("LIST_IMPORT_MEMBER_FEEDS"),
    retractall(fetched(_, _)),
    list_uri(ListURI),
    probe_member(FreshDID, FreshHandle), FreshDID = "did:plc:member-profile-fresh",
    probe_member(RecordedDID, RecordedHandle), RecordedDID = "did:plc:member-profile-recorded",
    fetchers(Fetchers),

    open_pg_connection(C0),
    delete_probe_rows(C0),
    list_payload(Payload),
    onGetList(pg_session(C0, C1), ListURI, Payload),

    % A membership recorded by an earlier, interrupted run.
    profile_pairs(ProfilePairs),
    onGetProfile(pg_session(C1, C2), props(ListURI, 10, 5, RecordedDID, ProfilePairs)),

    item_assoc(RecordedDID, RecordedHandle, RecordedAssoc),
    import_member(Fetchers, ListURI, RecordedAssoc, C2, C3),
    fetches(profile, RecordedDID, RecordedProfileFetches),
    memberships(C3, RecordedDID, RecordedMemberships),
    expect("an already recorded member's profile is not requested again",
        0, RecordedProfileFetches, 0, F1),
    expect("an already recorded member keeps a single membership",
        "1", RecordedMemberships, F1, F2),

    item_assoc(FreshDID, FreshHandle, FreshAssoc),
    import_member(Fetchers, ListURI, FreshAssoc, C3, C4),
    fetches(profile, FreshDID, FreshProfileFetches),
    memberships(C4, FreshDID, FreshMemberships),
    expect("a new member's profile is requested once",
        1, FreshProfileFetches, F2, F3),
    expect("a new member's membership is recorded",
        "1", FreshMemberships, F3, F4),

    import_member(Fetchers, ListURI, FreshAssoc, C4, C5),
    fetches(profile, FreshDID, FreshProfileFetchesAfterRerun),
    expect("re-importing a member just recorded does not request its profile again",
        1, FreshProfileFetchesAfterRerun, F4, F5),

    findall(A, fetched(feed, A), FeedFetches),
    expect("no member feed is fetched by default", [], FeedFetches, F5, F6),

    delete_probe_rows(C5),
    close_pg_connection(C5),
    (   F6 =:= 0
    ->  format("[OK] list_member_profile_test passed~n", []), halt(0)
    ;   format("[KO] list_member_profile_test: ~d failure(s)~n", [F6]), halt(1)
    ).
