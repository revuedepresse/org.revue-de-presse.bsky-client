:- module(synchronous_commit_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [
    close_pg_connection/1,
    open_pg_connection/1,
    pg_query_simple/3
]).

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

synchronous_commit(Setting) :-
    open_pg_connection(Conn),
    pg_query_simple(Conn, "SHOW synchronous_commit", data(_, [[Setting]])),
    close_pg_connection(Conn).

expect(Case, Expected) :-
    synchronous_commit(Setting),
    (   Setting == Expected
    ->  format("[ok] ~s: synchronous_commit = ~s~n", [Case, Setting])
    ;   format("[KO] ~s: synchronous_commit = ~w, expected ~s~n", [Case, Setting, Expected]), halt(1) ).

%% run_test/0
%
% Verifies that open_pg_connection/1 turns synchronous_commit off
% when PG_SYNCHRONOUS_COMMIT=off, and keeps the server's default
% (on) when the variable is unset.
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),
    unsetenv("PG_SYNCHRONOUS_COMMIT"),
    expect("unset", "on"),
    setenv("PG_SYNCHRONOUS_COMMIT", "off"),
    expect("PG_SYNCHRONOUS_COMMIT=off", "off"),
    unsetenv("PG_SYNCHRONOUS_COMMIT"),
    format("[OK] synchronous_commit_test passed~n", []),
    halt(0).
