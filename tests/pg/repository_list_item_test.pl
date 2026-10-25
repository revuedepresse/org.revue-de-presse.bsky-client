:- module(repository_list_item_test, [run_test/0]).

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
    pg_query_simple/3
]).
:- use_module('../../src/infrastructure/repository/repository_list_item', [
    event_by_screen_name/3,
    from_event/2,
    insert/3,
    insert_list_items_if_not_exists/3,
    record_by_screen_name/3
]).
:- use_module('../../src/serialization', [char_code_at/2]).

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

%% item_payload(-Payload)
%
% A getList item, encoded the way event_getListItem:onGetListItem/2
% encodes it before storing it.
item_payload(Payload) :-
    JSON = "{\"uri\":\"at://item-probe\",\"subject\":{\"did\":\"did:plc:item-probe\",\"handle\":\"item-probe.bsky.social\",\"displayName\":\"Item Probe\",\"avatar\":\"https://cdn.example/a.jpg\",\"associated\":{\"chat\":{\"allowIncoming\":\"all\"}},\"description\":\"Probe description\",\"createdAt\":\"2024-01-01T00:00:00Z\",\"indexedAt\":\"2024-01-01T00:00:00Z\"}}",
    phrase(json_chars(pairs(UnwrappedPairs)), JSON),
    write_term_to_chars(UnwrappedPairs, [quoted(true),double_quotes(true)], Chars),
    chars_utf8bytes(Chars, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, Utf8BytesPayload),
    chars_base64(Utf8BytesPayload, Payload, []).

%% expect_miss(+Goal, +ExpectedError, +Label)
expect_miss(Goal, ExpectedError, Label) :-
    catch(
        ( call(Goal),
          format("[KO] ~s on a miss succeeded instead of throwing~n", [Label]), halt(1) ),
        Error,
        (   Error = ExpectedError
        ->  format("[ok] ~s throws ~w on a miss~n", [Label, ExpectedError])
        ;   format("[KO] ~s on a miss threw ~w~n", [Label, Error]), halt(1) )
    ).

count_rows(Conn, SQL, Expected, Label) :-
    pg_query_simple(Conn, SQL, Reply),
    (   Reply = data(_, [[Expected]])
    ->  format("[ok] ~s: ~s row(s)~n", [Label, Expected])
    ;   format("[KO] ~s: expected ~s row(s), got ~w~n", [Label, Expected, Reply]), halt(1) ).

%% run_test/0
%
% Exercises the session-threaded repository_list_item API against the
% compose-managed Postgres:
%
%   1. event_by_screen_name/3 and record_by_screen_name/3 throw
%      cannot_read_rows_selected_by(screen_name) on a miss --
%      event_getListItem and insert_list_items_if_not_exists/3 rely on
%      that throw to insert.
%   2. insert/3 appends one member_profile_collected_event row; a second
%      call for the same handle reports it instead of duplicating it.
%   3. insert_list_items_if_not_exists/3 writes the weaving_user record
%      decoded by from_event/2, once.
%   4. Both lookups then return the stored rows.
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    open_pg_connection(C0),
    pg_query_simple(C0, "DELETE FROM member_profile_collected_event WHERE screen_name = 'item-probe.bsky.social'", _),
    pg_query_simple(C0, "DELETE FROM weaving_user WHERE usr_twitter_username = 'item-probe.bsky.social'", _),
    Handle = "item-probe.bsky.social",

    expect_miss(repository_list_item:event_by_screen_name(pg_session(C0, _), Handle, _),
        cannot_read_rows_selected_by(screen_name), "event_by_screen_name/3"),
    expect_miss(repository_list_item:record_by_screen_name(pg_session(C0, _), Handle, _),
        cannot_read_rows_selected_by(screen_name), "record_by_screen_name/3"),

    item_payload(Payload),
    repository_list_item:insert(pg_session(C0, C1), row(Handle, Payload), FirstResult),
    repository_list_item:insert(pg_session(C1, C2), row(Handle, Payload), SecondResult),
    (   FirstResult == ok, SecondResult == ok
    ->  format("[ok] both event inserts returned ok~n", [])
    ;   format("[KO] event inserts returned ~w, ~w~n", [FirstResult, SecondResult]), halt(1) ),
    count_rows(C2,
        "SELECT count(*)::int FROM member_profile_collected_event WHERE screen_name = 'item-probe.bsky.social'",
        "1", "profile events after two inserts"),

    repository_list_item:event_by_screen_name(pg_session(C2, C3), Handle, EventRows),
    (   EventRows = [EventRow],
        get_assoc(payload, EventRow, Payload)
    ->  format("[ok] event_by_screen_name/3 returns the stored payload~n", [])
    ;   format("[KO] event_by_screen_name/3 returned ~w~n", [EventRows]), halt(1) ),

    from_event(Payload, Row),
    (   Row = row(_Avatar, "Probe description", "did:plc:item-probe", Handle, "Item Probe")
    ->  format("[ok] from_event/2 decodes the stored payload~n", [])
    ;   format("[KO] from_event/2 returned ~w~n", [Row]), halt(1) ),

    repository_list_item:insert_list_items_if_not_exists(pg_session(C3, C4), Row, _),
    repository_list_item:insert_list_items_if_not_exists(pg_session(C4, C5), Row, _),
    count_rows(C5,
        "SELECT count(*)::int FROM weaving_user WHERE usr_twitter_username = 'item-probe.bsky.social' AND usr_twitter_id = 'did:plc:item-probe'",
        "1", "member records after two insert_list_items_if_not_exists calls"),

    repository_list_item:record_by_screen_name(pg_session(C5, C6), Handle, RecordRows),
    (   RecordRows = [RecordRow],
        get_assoc(twitter_username, RecordRow, Handle)
    ->  format("[ok] record_by_screen_name/3 returns the stored member~n", [])
    ;   format("[KO] record_by_screen_name/3 returned ~w~n", [RecordRows]), halt(1) ),

    close_pg_connection(C6),
    format("[OK] repository_list_item_test passed~n", []),
    halt(0).
