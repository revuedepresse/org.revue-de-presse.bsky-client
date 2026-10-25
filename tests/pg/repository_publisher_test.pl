:- module(repository_publisher_test, [run_test/0]).

:- use_module(library(assoc)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [
    close_pg_connection/1,
    open_pg_connection/1,
    pg_query/4,
    pg_query_simple/3
]).
:- use_module('../../src/infrastructure/repository/repository_publisher', [
    by_criteria/4,
    insert/3
]).

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

%% run_test/0
%
% Exercises the session-threaded repository_publisher API end-to-end
% against the compose-managed Postgres:
%
%   1. by_criteria/4 throws cannot_read_rows_selection before the row
%      exists -- event_getProfile relies on that throw to insert.
%   2. First insert/3 lands a new row. NextId is generated via next_id/2
%      and bound through $1::bigint; ListId (here passed as an integer)
%      is coerced to chars and bound through $2::bigint.
%   3. Second insert/3 with the same (name, screen_name) returns ok via
%      the duplicate path (no second INSERT).
%   4. Row count after both calls is exactly 1, list_id round-trips.
%   5. by_criteria/4 now returns the row, keyed by screen_name.
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    open_pg_connection(C0),
    pg_query_simple(C0, "DELETE FROM publishers_list WHERE name = 'at://publisher-probe'", _),

    catch(
        ( repository_publisher:by_criteria(pg_session(C0, _),
              list_uri("at://publisher-probe"), did("did:plc:publisher-probe"), MissingRows),
          format("[KO] by_criteria/4 before insert returned ~w~n", [MissingRows]), halt(1) ),
        MissError,
        (   MissError = cannot_read_rows_selection
        ->  format("[ok] by_criteria/4 throws cannot_read_rows_selection on a miss~n", [])
        ;   format("[KO] by_criteria/4 on a miss threw ~w~n", [MissError]), halt(1) )
    ),

    repository_publisher:insert(pg_session(C0, C1),
        row(42, "at://publisher-probe", _, _, "did:plc:publisher-probe", _),
        FirstResult),
    (   FirstResult == ok
    ->  format("[ok] first publisher insert returned ok~n", [])
    ;   format("[KO] first insert returned ~w~n", [FirstResult]), halt(1) ),

    repository_publisher:insert(pg_session(C1, C2),
        row(42, "at://publisher-probe", _, _, "did:plc:publisher-probe", _),
        SecondResult),
    (   SecondResult == ok
    ->  format("[ok] duplicate publisher insert returned ok (no throw)~n", [])
    ;   format("[KO] duplicate insert returned ~w~n", [SecondResult]), halt(1) ),

    pg_query_simple(C2,
        "SELECT count(*)::int FROM publishers_list WHERE name = 'at://publisher-probe'",
        CountReply),
    (   CountReply = data(_, [["1"]])
    ->  format("[ok] publisher row count after duplicate is 1~n", [])
    ;   format("[KO] expected 1 publisher row, got ~w~n", [CountReply]), halt(1) ),

    pg_query(C2,
        "SELECT id::text, list_id::text FROM publishers_list WHERE name = $1 AND screen_name = $2",
        ["at://publisher-probe", "did:plc:publisher-probe"],
        Reply),
    (   Reply = data([[[_|_], "42"|_]|_])
    ->  format("[ok] $2::bigint round-trip preserved the integer literal~n", [])
    ;   format("[KO] verification SELECT returned ~w~n", [Reply]), halt(1) ),

    repository_publisher:by_criteria(pg_session(C2, C3),
        list_uri("at://publisher-probe"), did("did:plc:publisher-probe"), Rows),
    (   Rows = [Row],
        get_assoc(screen_name, Row, "did:plc:publisher-probe")
    ->  format("[ok] by_criteria/4 returns the stored row~n", [])
    ;   format("[KO] by_criteria/4 returned ~w~n", [Rows]), halt(1) ),

    close_pg_connection(C3),
    format("[OK] repository_publisher_test passed~n", []),
    halt(0).
