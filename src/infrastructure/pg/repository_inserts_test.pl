:- module(repository_inserts_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('connection', [pg_query_simple/2]).
:- use_module('../repository/repository_status', [insert/3]).
:- use_module('../repository/repository_popularity', [insert_without_unicity_check/2]).

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

%% run_test/0
%
% Exercises the converted repository inserts end-to-end:
%   1. repository_status:insert/3 - first call writes a new row and
%      returns the ust_id via RETURNING; second call with the same
%      (handle, URI) is a no-op via ON CONFLICT DO NOTHING but still
%      yields the same ust_id (fetched by the follow-up SELECT).
%   2. repository_popularity:insert_without_unicity_check/2 - both
%      calls append (no UNIQUE constraint), leaving two rows.
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    pg_query_simple("DELETE FROM status_popularity WHERE publication_id = 'at://repo-probe'", _),
    pg_query_simple("DELETE FROM weaving_status    WHERE ust_status_id  = 'at://repo-probe'", _),

    repository_status:insert(
        row(_DN1, "alice.bsky.social", "hello", "avatar.png", "{}", "at://repo-probe", "2025-01-01T00:00:00Z"),
        StatusR1, UstId1),
    format("[ok] first status insert: ~w, ust_id=~s~n", [StatusR1, UstId1]),

    repository_status:insert(
        row(_DN2, "alice.bsky.social", "different text", "avatar.png", "{}", "at://repo-probe", "2025-01-02T00:00:00Z"),
        StatusR2, UstId2),
    format("[ok] second status insert: ~w, ust_id=~s~n", [StatusR2, UstId2]),
    (   UstId1 == UstId2
    ->  format("[ok] status is idempotent on (handle,uri) hash~n", [])
    ;   format("[KO] status duplicate returned different ust_id: ~s vs ~s~n", [UstId1, UstId2]),
        halt(1) ),

    pg_query_simple("SELECT count(*)::int FROM weaving_status WHERE ust_status_id = 'at://repo-probe'", StatusCount),
    (   StatusCount = data(_, [["1"]])
    ->  format("[ok] status row count after dup is 1~n", [])
    ;   format("[KO] expected 1 status row, got: ~w~n", [StatusCount]), halt(1) ),

    repository_popularity:insert_without_unicity_check(
        row(UstId1, "at://repo-probe", 5, 3), _Pop1),
    repository_popularity:insert_without_unicity_check(
        row(UstId1, "at://repo-probe", 7, 4), _Pop2),
    format("[ok] two popularity snapshots inserted~n", []),

    pg_query_simple("SELECT count(*)::int FROM status_popularity WHERE publication_id = 'at://repo-probe'", PopCount),
    (   PopCount = data(_, [["2"]])
    ->  format("[OK] repository_inserts_test passed: 1 status + 2 popularity snapshots~n", []),
        halt(0)
    ;   format("[KO] expected 2 popularity rows, got: ~w~n", [PopCount]), halt(1) ).
