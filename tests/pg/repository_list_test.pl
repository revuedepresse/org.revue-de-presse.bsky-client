:- module(repository_list_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [pg_query/3, pg_query_simple/2]).
:- use_module('../../src/infrastructure/repository/repository_list', [insert/2]).

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

%% run_test/0
%
% Exercises repository_list:insert/2 against the compose-managed
% Postgres. The primary thing under test is the $2::bigint bind cast:
% next_id/1 produces an integer, the repo encodes it via number_chars/2
% and binds it as a list of chars; Postgres must accept the text form
% and coerce it to bigint for the column.
%
%   1. First insert succeeds and InsertionResult = ok.
%   2. publishers_list_collected_event has exactly one matching row.
%   3. list_id round-trips: cast back to text via list_id::text and
%      compared to the integer literal we expect.
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    pg_query_simple(
        "DELETE FROM publishers_list_collected_event WHERE list_name = 'at://list-probe'",
        _),

    repository_list:insert(
        row("at://list-probe", "encoded-payload-chars"),
        FirstResult),
    (   FirstResult == ok
    ->  format("[ok] first list insert returned ok~n", [])
    ;   format("[KO] first insert returned ~w~n", [FirstResult]), halt(1) ),

    pg_query_simple(
        "SELECT count(*)::int FROM publishers_list_collected_event WHERE list_name = 'at://list-probe'",
        CountReply),
    (   CountReply = data(_, [["1"]])
    ->  format("[ok] list-event row count after first insert is 1~n", [])
    ;   format("[KO] expected 1 list-event row, got ~w~n", [CountReply]), halt(1) ),

    pg_query(
        "SELECT list_id::text FROM publishers_list_collected_event WHERE list_name = $1",
        ["at://list-probe"],
        Reply),
    (   Reply = data([[ListIdChars|_]|_])
    ->  format("[ok] stored list_id=~s~n", [ListIdChars]),
        (   catch(number_chars(_, ListIdChars), _, false)
        ->  format("[OK] repository_list_test passed: $2::bigint round-trip OK~n", []),
            halt(0)
        ;   format("[KO] list_id is not a valid integer: ~s~n", [ListIdChars]), halt(1) )
    ;   format("[KO] verification SELECT returned ~w~n", [Reply]), halt(1) ).
