:- module(repository_publisher_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [pg_query/3, pg_query_simple/2]).
:- use_module('../../src/infrastructure/repository/repository_publisher', [insert/2]).

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

%% run_test/0
%
% Exercises repository_publisher:insert/2 end-to-end against the
% compose-managed Postgres:
%
%   1. First insert lands a new row. NextId is generated via next_id/1
%      and bound through $1::bigint; ListId (here passed as an integer)
%      is coerced to chars and bound through $2::bigint. Verifies that
%      Postgres accepts text-format integers for bigint columns.
%   2. Second insert with the same (name, screen_name) returns ok via
%      the duplicate path (count_matching_records detects 1 match,
%      no second INSERT, follow-up SELECT returns the existing id).
%   3. Row count after both calls is exactly 1.
%   4. The stored id and list_id are real bigints (cast back to text
%      and compared char-by-char to the original integer literals).
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    pg_query_simple("DELETE FROM publishers_list WHERE name = 'at://publisher-probe'", _),

    repository_publisher:insert(
        row(42, "at://publisher-probe", _, _, "did:plc:publisher-probe", _),
        FirstResult),
    (   FirstResult == ok
    ->  format("[ok] first publisher insert returned ok~n", [])
    ;   format("[KO] first insert returned ~w~n", [FirstResult]), halt(1) ),

    repository_publisher:insert(
        row(42, "at://publisher-probe", _, _, "did:plc:publisher-probe", _),
        SecondResult),
    (   SecondResult == ok
    ->  format("[ok] duplicate publisher insert returned ok (no throw)~n", [])
    ;   format("[KO] duplicate insert returned ~w~n", [SecondResult]), halt(1) ),

    pg_query_simple(
        "SELECT count(*)::int FROM publishers_list WHERE name = 'at://publisher-probe'",
        CountReply),
    (   CountReply = data(_, [["1"]])
    ->  format("[ok] publisher row count after duplicate is 1~n", [])
    ;   format("[KO] expected 1 publisher row, got ~w~n", [CountReply]), halt(1) ),

    pg_query(
        "SELECT id::text, list_id::text FROM publishers_list WHERE name = $1 AND screen_name = $2",
        ["at://publisher-probe", "did:plc:publisher-probe"],
        Reply),
    (   Reply = data([[IdChars, ListIdChars|_]|_])
    ->  format("[ok] stored id=~s list_id=~s~n", [IdChars, ListIdChars]),
        (   ListIdChars == "42"
        ->  format("[ok] $2::bigint round-trip preserved the integer literal~n", [])
        ;   format("[KO] list_id round-trip mismatch: got ~s expected 42~n", [ListIdChars]),
            halt(1) ),
        (   IdChars = [_|_]
        ->  format("[OK] repository_publisher_test passed~n", []), halt(0)
        ;   format("[KO] id came back empty: ~w~n", [IdChars]), halt(1) )
    ;   format("[KO] verification SELECT returned ~w~n", [Reply]), halt(1) ).
