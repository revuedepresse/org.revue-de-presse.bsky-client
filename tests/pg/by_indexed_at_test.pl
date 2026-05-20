:- module(by_indexed_at_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [pg_query_simple/2]).
:- use_module('../../src/infrastructure/repository/repository_status', [
    by_indexed_at/2,
    insert/3
]).

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

%% run_test/0
%
% Reproducer for the production SIGSEGV that fires while
% `repository_status:by_indexed_at/2` runs on the first post of a
% page (right after the prior page's INSERT chain). The pattern under
% test mirrors the worker's per-post flow:
%
%   by_indexed_at SELECT -> insert (RETURNING + follow-up SELECT) -> repeat
%
% The test seeds a few rows then drives the SELECT / INSERT / SELECT
% chain repeatedly against the same pooled connection so any
% cumulative connection-state corruption in the postgresql-prolog
% wire client surfaces as either an unexpected reply shape, a
% throw, or a clean SIGSEGV (in which case the test process dies
% with signal 11 instead of returning 0/1).
run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    Handle = "by-indexed-at-probe.bsky.social",
    pg_query_simple("DELETE FROM status_popularity WHERE publication_id LIKE 'at://by-indexed-at-probe/%'", _),
    pg_query_simple("DELETE FROM weaving_status WHERE ust_name = 'by-indexed-at-probe.bsky.social'", _),

    seed_row(Handle, "at://by-indexed-at-probe/a", "2026-05-19T09:39:08.000Z", _UstId1),
    seed_row(Handle, "at://by-indexed-at-probe/b", "2026-05-19T10:00:00.000Z", _UstId2),
    format("[ok] seeded two rows~n", []),

    test_lookup_finds_seeded_row(Handle),
    test_lookup_misses_unknown_timestamp(Handle),
    test_lookup_loop_then_insert(Handle, 20),

    format("[OK] by_indexed_at_test passed~n", []),
    halt(0).

seed_row(Handle, URI, CreatedAt, UstId) :-
    repository_status:insert(
        row(_DisplayName, Handle, "probe text", "avatar.png", "{}", URI, CreatedAt),
        _Result,
        UstId).

% Looking up a seeded row by its exact (handle, indexed_at) must
% return one row. The IndexedAt argument is an ISO-8601 chars
% list; by_indexed_at/2 rewrites it to the YYYY-MM-DD HH:MM:SS
% form Postgres expects.
test_lookup_finds_seeded_row(Handle) :-
    by_indexed_at(
        indexed_at("2026-05-19T09:39:08.000Z")-handle(Handle),
        HeadersAndRows),
    length(HeadersAndRows, N),
    (   N >= 1
    ->  format("[ok] seeded row found (rows=~w)~n", [N])
    ;   format("[KO] seeded row not found, rows=~w~n", [N]), halt(1) ).

% Looking up a never-seeded timestamp must return zero rows.
test_lookup_misses_unknown_timestamp(Handle) :-
    by_indexed_at(
        indexed_at("2020-01-01T00:00:00.000Z")-handle(Handle),
        HeadersAndRows),
    length(HeadersAndRows, N),
    (   N == 0
    ->  format("[ok] unknown timestamp returns no rows~n", [])
    ;   format("[KO] unknown timestamp returned ~w rows~n", [N]), halt(1) ).

% Drive the SELECT/INSERT cycle N times on the cached connection.
% If the wire client accumulates bad state across queries this
% will either misbehave or trip the SIGSEGV the production worker
% sees on the second page of a feed traversal.
test_lookup_loop_then_insert(Handle, N) :-
    loop_cycles(Handle, 1, N),
    format("[ok] survived ~w SELECT/INSERT/SELECT cycles~n", [N]).

loop_cycles(_Handle, I, N) :- I > N.
loop_cycles(Handle, I, N) :-
    I =< N,
    number_chars(I, IChars),
    append(["at://by-indexed-at-probe/loop-", IChars], URI),
    by_indexed_at(
        indexed_at("2026-05-19T09:39:08.000Z")-handle(Handle),
        _Existing),
    repository_status:insert(
        row(_DN, Handle, "loop probe", "avatar.png", "{}", URI, "2026-05-19T11:00:00.000Z"),
        _R, _UstId),
    by_indexed_at(
        indexed_at("2026-05-19T11:00:00.000Z")-handle(Handle),
        _After),
    I1 is I + 1,
    loop_cycles(Handle, I1, N).
