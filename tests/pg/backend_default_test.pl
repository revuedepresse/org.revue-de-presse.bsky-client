:- module(backend_default_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(os)).

:- use_module('../../src/configuration', [pg_backend/1]).

/*
Asserts that pg_backend/1 falls back to "wire" when PG_BACKEND is
unset. CI runs this without PG_BACKEND in the step env, so the
unset branch is exercised end-to-end.

A regression of the default clause in configuration:pg_backend/1
would cause every repository dispatch to fail at its guard and
surface as a flood of clause failures - this test catches it on
its own first.
*/

%% run_test/0
%
% Two checks:
%   1. PG_BACKEND is actually unset in the test process. If a parent
%      shell has it set, this test would silently prove nothing.
%   2. pg_backend/1 returns "wire" given that unset state.
run_test :-
    (   getenv("PG_BACKEND", Set)
    ->  format("[KO] PG_BACKEND must be unset for this test; got ~w~n", [Set]),
        halt(2)
    ;   format("[ok] PG_BACKEND is unset~n", []) ),

    (   pg_backend("wire")
    ->  format("[OK] backend_default_test passed: pg_backend(\"wire\") succeeded with PG_BACKEND unset~n", []),
        halt(0)
    ;   format("[KO] pg_backend(\"wire\") failed even though PG_BACKEND is unset~n", []),
        halt(1) ).
