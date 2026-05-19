:- module(idempotence_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [pg_query/3, pg_query_simple/2]).

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

%% run_test/0
%
% Verifies that bind-param INSERT ... ON CONFLICT (hash) DO NOTHING
% RETURNING legacy_id is idempotent: first call inserts and returns the
% legacy_id row; second call with the same hash returns no rows.
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    pg_query_simple("DELETE FROM publication WHERE hash = 'idempotence-probe'", _),

    InsertSQL = "INSERT INTO publication (id, legacy_id, hash, screen_name, text, avatar_url, document_id, document, published_at) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9) ON CONFLICT (hash) DO NOTHING RETURNING legacy_id",
    Params1 = ["uuid-1", "100", "idempotence-probe", "alice.bsky.social", "hello", "avatar.png", "at://uri/1", "{}", "2025-01-01T00:00:00Z"],
    Params2 = ["uuid-2", "200", "idempotence-probe", "alice.bsky.social", "world", "avatar.png", "at://uri/2", "{}", "2025-01-02T00:00:00Z"],

    pg_query(InsertSQL, Params1, R1),
    format("[..] first insert returned: ~w~n", [R1]),
    (   R1 = data([[CsLegacyId|_]])
    ->  format("[ok] first insert returned legacy_id ~s~n", [CsLegacyId])
    ;   format("[KO] expected one row from first INSERT, got: ~w~n", [R1]), halt(1) ),

    pg_query(InsertSQL, Params2, R2),
    format("[..] second insert returned: ~w~n", [R2]),
    (   R2 = data([])
    ->  format("[ok] second insert was a no-op via ON CONFLICT~n", [])
    ;   format("[KO] expected zero rows from duplicate INSERT, got: ~w~n", [R2]), halt(1) ),

    pg_query_simple("SELECT count(*)::int FROM publication WHERE hash = 'idempotence-probe'", R3),
    (   R3 = data(_, [["1"]])
    ->  format("[OK] idempotence_test passed: exactly one row in publication~n", []), halt(0)
    ;   format("[KO] expected count = 1, got: ~w~n", [R3]), halt(1) ).
