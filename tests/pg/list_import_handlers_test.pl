:- module(list_import_handlers_test, [run_test/0]).

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
:- use_module('../../src/domain/events/app/bsky/graph/event_getList', [onGetList/3]).
:- use_module('../../src/domain/events/app/bsky/graph/event_getListItem', [onGetListItem/3]).
:- use_module('../../src/domain/events/app/bsky/actor/event_getProfile', [onGetProfile/2]).
:- use_module('../../src/serialization', [
    char_code_at/2,
    pairs_to_assoc/2,
    to_json_chars/2,
    wrapped_pairs_to_assoc/2
]).

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

list_uri("at://did:plc:import-probe/app.bsky.graph.list/probe").

%% list_body(-BodyChars)
%
% A one-member getList response, shaped like the app view's.
list_body(BodyChars) :-
    list_uri(ListURI),
    append([
        "{\"list\":{\"uri\":\"", ListURI, "\",\"name\":\"import-probe\"},",
        "\"items\":[{\"uri\":\"at://did:plc:import-probe/app.bsky.graph.listitem/1\",",
        "\"subject\":{\"did\":\"did:plc:member-probe\",\"handle\":\"member-probe.bsky.social\",",
        "\"displayName\":\"Member Probe\",\"avatar\":\"https://cdn.example/m.jpg\",",
        "\"associated\":{\"chat\":{\"allowIncoming\":\"all\"}},",
        "\"description\":\"A member\",\"createdAt\":\"2024-01-01T00:00:00Z\",",
        "\"indexedAt\":\"2024-01-01T00:00:00Z\"}}]}"
    ], BodyChars).

%% list_payload(+BodyChars, -Payload)
%
% Same encoding as getList.pl's payload/2.
list_payload(BodyChars, Payload) :-
    to_json_chars(BodyChars, JSONChars),
    write_term_to_chars(JSONChars, [quoted(false),double_quotes(true)], Chars),
    chars_utf8bytes(Chars, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, Utf8BytesPayload),
    chars_base64(Utf8BytesPayload, Payload, []).

single_value(Conn, SQL, Params, Value) :-
    pg_query(Conn, SQL, Params, Reply),
    (   Reply = data([[Value]])
    ->  true
    ;   format("[KO] ~s returned ~w~n", [SQL, Reply]), halt(1) ).

expect(Conn, SQL, Params, Expected, Label) :-
    single_value(Conn, SQL, Params, Value),
    (   Value == Expected
    ->  format("[ok] ~s: ~s~n", [Label, Value])
    ;   format("[KO] ~s: expected ~s, got ~s~n", [Label, Expected, Value]), halt(1) ).

%% run_test/0
%
% Drives the three list-import handlers the way getList.pl does, on
% one connection, and checks the rows and their relationships:
%
%   1. onGetList/3 records the list once (a second call finds it).
%   2. onGetProfile/2 adds the member to publishers_list under the
%      list's URI and list_id; a second call throws
%      pre_existing_publisher/1 instead of inserting again.
%   3. onGetListItem/3 records the member's profile event and its
%      weaving_user record, keyed by handle and DID.
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME","TEMP_DIR"]),
    list_uri(ListURI),

    open_pg_connection(C0),
    pg_query(C0, "DELETE FROM publishers_list_collected_event WHERE list_name = $1", [ListURI], _),
    pg_query(C0, "DELETE FROM publishers_list WHERE name = $1", [ListURI], _),
    pg_query_simple(C0, "DELETE FROM member_profile_collected_event WHERE screen_name = 'member-probe.bsky.social'", _),
    pg_query_simple(C0, "DELETE FROM weaving_user WHERE usr_twitter_username = 'member-probe.bsky.social'", _),

    list_body(BodyChars),
    list_payload(BodyChars, Payload),
    onGetList(pg_session(C0, C1), ListURI, Payload),
    onGetList(pg_session(C1, C2), ListURI, Payload),
    expect(C2, "SELECT count(*)::text FROM publishers_list_collected_event WHERE list_name = $1",
        [ListURI], "1", "list events after two onGetList calls"),
    single_value(C2, "SELECT list_id::text FROM publishers_list_collected_event WHERE list_name = $1",
        [ListURI], ListIdChars),

    phrase(json_chars(pairs(ResponsePairs)), BodyChars),
    pairs_to_assoc(ResponsePairs, JSONAssoc),
    get_assoc(items, JSONAssoc, [Item]),
    wrapped_pairs_to_assoc(Item, ItemAssoc),

    ProfilePairs = [string("followersCount")-number("10"), string("followsCount")-number("5")],
    onGetProfile(pg_session(C2, C3), props(ListURI, 10, 5, "did:plc:member-probe", ProfilePairs)),
    expect(C3, "SELECT count(*)::text FROM publishers_list WHERE name = $1 AND screen_name = $2",
        [ListURI, "did:plc:member-probe"], "1", "publishers_list rows for the member"),
    expect(C3, "SELECT list_id::text FROM publishers_list WHERE name = $1 AND screen_name = $2",
        [ListURI, "did:plc:member-probe"], ListIdChars, "member's list_id matches the list event's"),
    catch(
        ( onGetProfile(pg_session(C3, _), props(ListURI, 10, 5, "did:plc:member-probe", ProfilePairs)),
          format("[KO] a second onGetProfile/2 did not report the existing member~n", []), halt(1) ),
        Thrown,
        (   Thrown = pre_existing_publisher(_)
        ->  format("[ok] a second onGetProfile/2 throws pre_existing_publisher/1~n", [])
        ;   format("[KO] a second onGetProfile/2 threw ~w~n", [Thrown]), halt(1) )
    ),

    onGetListItem(pg_session(C3, C4), ItemAssoc, Item),
    onGetListItem(pg_session(C4, C5), ItemAssoc, Item),
    expect(C5, "SELECT count(*)::text FROM member_profile_collected_event WHERE screen_name = $1",
        ["member-probe.bsky.social"], "1", "profile events for the member"),
    expect(C5, "SELECT count(*)::text FROM weaving_user WHERE usr_twitter_username = $1 AND usr_twitter_id = $2",
        ["member-probe.bsky.social", "did:plc:member-probe"], "1", "weaving_user records for the member"),

    close_pg_connection(C5),
    format("[OK] list_import_handlers_test passed~n", []),
    halt(0).
