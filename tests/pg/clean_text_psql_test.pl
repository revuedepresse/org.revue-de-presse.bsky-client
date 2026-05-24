:- module(clean_text_psql_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [pg_query_simple/2]).
:- use_module('../../src/infrastructure/repository/repository_status', [insert/3]).
:- use_module('../../src/clean_text', [clean_text/2]).
:- use_module('../../src/configuration', [pg_backend/1]).

/*
End-to-end coverage that the psql clause of repository_status:insert/3
calls clean_text/2 before persisting the post body.

CI runs this with PG_BACKEND=psql in the step env, so the dispatcher
in repository_status selects the shell-out path. The wire client is
used only for teardown (DELETE the probe row) and verification
(SELECT the persisted ust_text) - same DB, two transports.

A regression where the psql clause forgets to call clean_text/2 would
leave the raw "CafÃ©" mojibake on disk and this test would fail
on the assertion that ust_text == "Café".
*/

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

%% run_test/0
%
% 1. Assert we're actually running on the psql backend; otherwise this
%    test would silently fall through to the wire path and prove nothing.
% 2. DELETE any previous probe row keyed on a known URI.
% 3. INSERT a row with a known mojibake-laden body. The repository's
%    psql clause must clean_text/2 the body before persistence.
% 4. SELECT the persisted ust_text and assert it equals what
%    clean_text/2 produces from the same raw input.
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    (   pg_backend("psql")
    ->  format("[ok] PG_BACKEND=psql active~n", [])
    ;   format("[KO] PG_BACKEND is not psql; this test must run under psql~n", []),
        halt(2) ),

    ProbeURI = "at://clean-text-psql-probe",
    DirtyBody = "\"CafÃ©\"",
    clean_text(DirtyBody, ExpectedClean),

    pg_query_simple("DELETE FROM weaving_status WHERE ust_status_id = 'at://clean-text-psql-probe'", _),

    insert(
        row(_DN, "alice.bsky.social", DirtyBody, "avatar.png", "{}",
            ProbeURI, "2025-01-01T00:00:00Z"),
        _InsertResult, _UstId),
    format("[ok] insert through psql clause completed~n", []),

    pg_query_simple(
        "SELECT ust_text FROM weaving_status WHERE ust_status_id = 'at://clean-text-psql-probe'",
        Reply),
    (   Reply = data(_, [[Persisted]])
    ->  format("[ok] read back persisted text~n", [])
    ;   format("[KO] unexpected reply shape: ~w~n", [Reply]), halt(1) ),

    (   Persisted == ExpectedClean
    ->  format("[OK] clean_text_psql_test passed: text was cleaned before psql insert~n", []),
        halt(0)
    ;   format("[KO] expected ~w, got ~w~n", [ExpectedClean, Persisted]),
        halt(1) ).
