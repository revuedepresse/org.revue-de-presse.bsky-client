:- module(segv_replay_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [pg_query_simple/2]).
:- use_module('../../src/infrastructure/repository/repository_status', [
    by_indexed_at/2
]).

/*
Replay the last captured bind-param set against the live wire
client to try to reproduce the scryer unify_constant SIGSEGV
without going through the Bluesky API again.

The captures are written from inside the worker:

  - src/infrastructure/repository/repository_status.pl
    `by_indexed_at/2` writes
    /tmp/segv-investigation/last-by-indexed-at.pl just before
    calling matching_criteria/4. On a crash mid-call the file
    holds the exact inputs that were in flight.

  - src/app/bsky/feed/getAuthorFeed.pl
    `send_request/3` writes
    /tmp/segv-investigation/last-feed-body.txt and
    /tmp/segv-investigation/last-feed-pairs.pl right after the
    JSON-DCG parse, so on a later crash the file holds the most
    recent HTTP body and parsed pair tree.

This test only consumes the last-by-indexed-at capture: it loads
the captured `last_by_indexed_at/1` term and runs by_indexed_at/2
once with the same Handle and IndexedAt. If the wire path is
self-sufficient to reproduce the crash, this test will SIGSEGV
the process; if reproducing requires the prior HTTP/JSON path
to have run in the same process (the suspected case), this
test will return cleanly.

Read-only -- no INSERT/UPDATE/DELETE.
*/

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

capture_path("/tmp/segv-investigation/last-by-indexed-at.pl").

load_capture(Term) :-
    capture_path(Path),
    open(Path, read, Stream),
    catch(
        read_term(Stream, Term, []),
        E,
        (close(Stream), throw(E))
    ),
    close(Stream).

run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    capture_path(Path),
    (   file_exists(Path)
    ->  true
    ;   format("[KO] no capture file at ~s; run the worker first~n", [Path]),
        halt(2)
    ),

    load_capture(Captured),
    Captured = last_by_indexed_at(
        handle(Handle),
        indexed_at(IndexedAt),
        indexed_at_date(_IndexedAtDate),
        sql(_SQL)
    ),

    atom_chars(HandleAtom, Handle),
    atom_chars(IndexedAtAtom, IndexedAt),
    format("[..] replaying by_indexed_at(indexed_at(~q)-handle(~q))~n",
           [IndexedAtAtom, HandleAtom]),

    catch(
        ( by_indexed_at(
              indexed_at(IndexedAt)-handle(Handle),
              HeadersAndRows)
        ->
              length(HeadersAndRows, N),
              format("[OK] segv_replay survived; predicate returned ~w rows~n", [N]),
              halt(0)
        ;
              format("[!!] segv_replay survived; predicate FAILED -- this is the patched-binary signal: the defensive guard fired~n", []),
              halt(0)
        ),
        Err,
        ( format("[!!] segv_replay survived; predicate THREW: ~q~n", [Err]),
          halt(0) )
    ).

% library(files):file_exists/1 not always available; reimplement
% via open-on-read which fails cleanly if the file is missing.
file_exists(Path) :-
    catch(
        ( open(Path, read, S), close(S) ),
        _,
        fail
    ).
