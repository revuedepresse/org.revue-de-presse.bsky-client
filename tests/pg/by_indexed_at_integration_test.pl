:- module(by_indexed_at_integration_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [pg_query_simple/2]).
:- use_module('../../src/infrastructure/repository/repository_status', [
    by_indexed_at/2
]).

/*
Integration smoke test for repository_status:by_indexed_at/2
against production data via .env.local.

Pulls a sample of recent (ust_name, ust_created_at) pairs from
weaving_status, then drives by_indexed_at/2 once per pair while
tallying outcomes:

  * `ok`    -- predicate returned with HeadersAndRows
  * `fail`  -- predicate returned false; on a patched scryer-prolog
               binary that is the defensive guard firing instead
               of a SIGSEGV
  * `throw` -- predicate threw any other error

Reproduction status
-------------------

This is NOT a reliable reproducer for the SIGSEGV in
scryer_prolog::machine::unify::Unifier::unify_constant. Empirical
data so far:

  * The crash only fires when the worker has just consumed a
    Bluesky getAuthorFeed JSON response (HTTP + JSON DCG) in the
    same scryer process, *then* runs the SELECT path. The
    Postgres SELECTs alone do not reproduce it.
  * The reliable reproducer remains `AUTHOR=<handle> make
    app__bsky__feed__getAuthorFeed`, which on its bad-data days
    crashes on roughly 72 % of the configured French press author
    handles. The data window drifts hour by hour; this test
    therefore mostly comes back green even against an unpatched
    binary.
  * The hermetic reproducer is the Rust unit test
    `machine::unify::tests::unify_constant_rejects_bogus_arena_ptr_below_first_page`
    in deps/scryer-prolog: it constructs the exact bogus cell
    observed in production and red-green-verifies the defensive
    guard.

The test stays in tree as a periodic sanity check against the
production database (read-only, no writes) and as a place to
land a stronger reproducer if one is found.

Read-only: by_indexed_at/2 only runs SELECT statements; this
test does not modify any production data.
*/

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    format("[..] sampling recent weaving_status rows~n", []),
    sample_recent_pairs(Pairs),
    length(Pairs, NSamples),
    (   NSamples > 0
    ->  format("[ok] sampled ~w (handle, indexed_at) pairs~n", [NSamples])
    ;   format("[KO] no rows in weaving_status to sample from~n", []),
        halt(1)
    ),

    exercise_lookups(Pairs, 0-0-0, Ok-Fail-Throw),

    format("[OK] integration run survived: ok=~w fail=~w throw=~w (over ~w samples)~n",
           [Ok, Fail, Throw, NSamples]),
    halt(0).

% Sample up to 50 recent rows. ust_created_at is rewritten to the
% ISO-8601-with-T-separator chars list that by_indexed_at/2
% expects (length 10 prefix, T, length 8 suffix, .fractional Z).
% by_indexed_at/2 parses IndexedAt as a length-10 date prefix,
% one separator char, a length-8 HH:MM:SS suffix, and anything
% trailing. Postgres's default ::text rendering of a timestamp
% ('2026-05-19 09:39:08') matches that shape exactly, so no
% to_char rewriting is needed.
sample_recent_pairs(Pairs) :-
    SQL = "SELECT ust_name, ust_created_at::text FROM public.weaving_status ORDER BY ust_id DESC LIMIT 50",
    pg_query_simple(SQL, Reply),
    extract_pairs(Reply, Pairs).

extract_pairs(data(_Cols, Rows), Pairs) :-
    maplist(row_to_pair, Rows, Pairs).
extract_pairs(data(Rows), Pairs) :-
    maplist(row_to_pair, Rows, Pairs).

row_to_pair([Handle, IndexedAt|_], pair(Handle, IndexedAt)).

exercise_lookups([], Acc, Acc).
exercise_lookups([pair(Handle, IndexedAt)|Rest], Ok0-Fail0-Throw0, Final) :-
    catch(
        ( (   by_indexed_at(
                  indexed_at(IndexedAt)-handle(Handle),
                  _HeadersAndRows)
          ->  Ok1 is Ok0 + 1, Fail1 = Fail0, Throw1 = Throw0
          ;   Ok1 = Ok0,      Fail1 is Fail0 + 1, Throw1 = Throw0
          )
        ),
        Err,
        ( Ok1 = Ok0, Fail1 = Fail0, Throw1 is Throw0 + 1,
          format("[!!] threw on ~s @ ~s: ~q~n",
                 [Handle, IndexedAt, Err])
        )
    ),
    exercise_lookups(Rest, Ok1-Fail1-Throw1, Final).
