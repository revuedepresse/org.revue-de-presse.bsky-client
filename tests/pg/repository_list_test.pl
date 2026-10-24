:- module(repository_list_test, [run_test/0]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [
    close_pg_connection/1,
    open_pg_connection/1,
    pg_query/4,
    pg_query_simple/3
]).
:- use_module('../../src/infrastructure/repository/repository_list', [
    by_list_uri/3,
    by_list_uri_or_throw/3,
    insert/3
]).
:- use_module('../../src/serialization', [
    char_code_at/2,
    to_json_chars/2
]).

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

%% list_payload(+ListURI, -Payload)
%
% Encodes a getList response the way getList.pl's payload/2 does, so
% by_list_uri_or_throw/3 decodes exactly what production stores.
list_payload(ListURI, Payload) :-
    append(["{\"list\":{\"uri\":\"", ListURI, "\",\"name\":\"probe\"},\"items\":[]}"], BodyChars),
    to_json_chars(BodyChars, JSONChars),
    write_term_to_chars(JSONChars, [quoted(false),double_quotes(true)], Chars),
    chars_utf8bytes(Chars, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, Utf8BytesPayload),
    chars_base64(Utf8BytesPayload, Payload, []).

%% run_test/0
%
% Exercises the session-threaded repository_list API against the
% compose-managed Postgres:
%
%   1. insert/3 lands one publishers_list_collected_event row and binds
%      list_id through $2::bigint (round-tripped back as an integer).
%   2. A second insert/3 for the same list is reported, not duplicated.
%   3. by_list_uri/3 returns the stored row for a known list URI.
%   4. by_list_uri/3 throws cannot_read_rows_selected_by(list_id) for an
%      unknown list URI -- event_getList relies on that throw to insert.
%   5. by_list_uri_or_throw/3 decodes the payload and merges list_id.
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    open_pg_connection(C0),
    pg_query_simple(C0,
        "DELETE FROM publishers_list_collected_event WHERE list_name LIKE 'at://list-probe%'",
        _),

    list_payload("at://list-probe", Payload),
    repository_list:insert(pg_session(C0, C1), row("at://list-probe", Payload), FirstResult),
    (   FirstResult == ok
    ->  format("[ok] first list insert returned ok~n", [])
    ;   format("[KO] first insert returned ~w~n", [FirstResult]), halt(1) ),

    repository_list:insert(pg_session(C1, C2), row("at://list-probe", Payload), SecondResult),
    (   SecondResult == ok
    ->  format("[ok] second list insert returned ok~n", [])
    ;   format("[KO] second insert returned ~w~n", [SecondResult]), halt(1) ),

    pg_query_simple(C2,
        "SELECT count(*)::int FROM publishers_list_collected_event WHERE list_name = 'at://list-probe'",
        CountReply),
    (   CountReply = data(_, [["1"]])
    ->  format("[ok] list-event row count after two inserts is 1~n", [])
    ;   format("[KO] expected 1 list-event row, got ~w~n", [CountReply]), halt(1) ),

    pg_query(C2,
        "SELECT list_id::text FROM publishers_list_collected_event WHERE list_name = $1",
        ["at://list-probe"],
        Reply),
    (   Reply = data([[ListIdChars|_]|_]),
        catch(number_chars(_, ListIdChars), _, false)
    ->  format("[ok] stored list_id=~s~n", [ListIdChars])
    ;   format("[KO] verification SELECT returned ~w~n", [Reply]), halt(1) ),

    repository_list:by_list_uri(pg_session(C2, C3), "at://list-probe", Rows),
    (   Rows = [Row],
        get_assoc(list_id, Row, ListIdChars)
    ->  format("[ok] by_list_uri/3 found the stored row~n", [])
    ;   format("[KO] by_list_uri/3 returned ~w~n", [Rows]), halt(1) ),

    catch(
        ( repository_list:by_list_uri(pg_session(C3, _), "at://list-probe-missing", MissingRows),
          format("[KO] by_list_uri/3 on a missing list returned ~w~n", [MissingRows]), halt(1) ),
        MissError,
        (   MissError = cannot_read_rows_selected_by(list_id)
        ->  format("[ok] by_list_uri/3 throws cannot_read_rows_selected_by(list_id) on a miss~n", [])
        ;   format("[KO] by_list_uri/3 on a miss threw ~w~n", [MissError]), halt(1) )
    ),

    repository_list:by_list_uri_or_throw(pg_session(C3, C4), list_uri("at://list-probe"), Assoc),
    (   get_assoc(list_id, Assoc, ListIdChars)
    ->  format("[ok] by_list_uri_or_throw/3 merged list_id into the decoded payload~n", [])
    ;   format("[KO] by_list_uri_or_throw/3 returned ~w~n", [Assoc]), halt(1) ),

    close_pg_connection(C4),
    format("[OK] repository_list_test passed~n", []),
    halt(0).
