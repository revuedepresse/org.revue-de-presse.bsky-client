:- module(scram_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../deps/postgresql-prolog/postgresql', [connect/6, query/3]).

env_or_die(Name, Value) :-
    (   getenv(Name, V)
    ->  Value = V
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2)
    ).

%% run_test/0
%
% End-to-end test: connect to a real PostgreSQL 17 over TCP using
% SCRAM-SHA-256, run a simple SELECT against the seed row inserted by
% docker/postgres/init/01-scram-probe.sql, and assert the round-trip.
run_test :-
    env_or_die("DATABASE_HOST", Host),
    env_or_die("DATABASE_PORT", PortChars),
    number_chars(Port, PortChars),
    env_or_die("DATABASE_USERNAME", User),
    env_or_die("DATABASE_PASSWORD", Pass),
    env_or_die("DATABASE_DB_NAME", DB),

    format("[..] connecting to ~s:~d as ~s -> ~s~n", [Host, Port, User, DB]),
    connect(User, Pass, Host, Port, DB, Conn),
    format("[ok] handshake completed (SCRAM-SHA-256)~n", []),

    query(Conn, "SELECT id::text, note FROM scram_probe WHERE id = 1", Result),
    format("[..] result: ~w~n", [Result]),

    (   Result = data(_Headers, [["1", "scram-sha-256 ok"]])
    ->  format("[OK] scram_test passed~n", []), halt(0)
    ;   format("[KO] unexpected result shape: ~w~n", [Result]), halt(1)
    ).
