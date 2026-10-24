:- module(pg_query_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [
    close_pg_connection/1,
    open_pg_connection/1,
    pg_query/4,
    pg_query_simple/3
]).

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

%% run_test/0
%
% Verifies that connection.pl: (a) opens a connection via
% SCRAM-SHA-256, (b) supports prepared statements via $1,$2 bind params,
% (c) runs several queries on the same explicitly threaded connection.
% Targets the seed schema created by docker/postgres/init/01-scram-probe.sql.
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    open_pg_connection(Conn),
    format("[ok] connection opened~n", []),

    pg_query(Conn, "INSERT INTO scram_probe (id, note) VALUES ($1, $2) ON CONFLICT (id) DO NOTHING",
             ["42", "from bind params"], InsResult),
    format("[ok] bind-param INSERT result: ~w~n", [InsResult]),

    pg_query(Conn, "SELECT note FROM scram_probe WHERE id = $1",
             ["42"], SelResult),
    format("[ok] bind-param SELECT result: ~w~n", [SelResult]),
    (   SelResult = data([["from bind params"]])
    ->  format("[ok] bind-param round-trip verified~n", [])
    ;   format("[KO] unexpected SELECT shape: ~w~n", [SelResult]), halt(1) ),

    pg_query_simple(Conn, "SELECT count(*)::int FROM scram_probe", SimpleResult),
    format("[ok] simple-protocol SELECT: ~w~n", [SimpleResult]),

    close_pg_connection(Conn),
    format("[OK] pg_query_test passed~n", []),
    halt(0).
