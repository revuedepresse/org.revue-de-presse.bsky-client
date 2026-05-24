:- module(sigsegv_repeat_pg_query_reproducer, [run_test/0]).

:- use_module(library(assoc)).
:- use_module(library(between)).
:- use_module(library(crypto)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../deps/postgresql-prolog/postgresql', [
    connect/6,
    query/4
]).
:- use_module('../../src/serialization', [
    pairs_to_assoc/2,
    wrapped_pairs_to_assoc/2
]).

/*
Reproducer scaffold for the scryer-prolog `unify_constant` SIGSEGV
that takes down the worker mid-iteration of the `onGetAuthorFeed`
maplist.

The fix has been identified
---------------------------

The upstream defensive fix lives at deps/scryer-prolog commit
`b02e0c47 fix: Guard Unifier::unify_constant against bogus arena
pointers`. It validates that the would-be arena pointer is above
the first 64 KiB of address space and is ArenaHeader-aligned
before dereferencing; on failure it sets `fail = true` and returns
so the unification surfaces as a clean Prolog failure that
catch/3 can handle, instead of a SIGSEGV.

fun.sh's `configure` now prepends `deps/scryer-prolog/target/
release` to PATH when a patched binary is present, so production
calls go through the fixed VM (v0.10.0-163-gb02e0c47).

This file is kept as a regression scaffold so the day someone
upgrades scryer past 0.10.0-163, a `make test-pg-sigsegv-repeat`
target re-checks whether the guard still holds on the same code
shape. It does NOT trigger the crash today on either binary --
see "Status against the production crash" below.

What this file does
-------------------

  1. Loads tests/fixtures/segv-investigation/last-feed-pairs.pl --
     the verbatim parsed JSON-DCG pair tree of a real getAuthorFeed
     response, captured at the moment of a prior crash. Same shape
     and size as what scryer holds in memory just before the
     production crash.

  2. Walks each post through wrapped_pairs_to_assoc/2 and
     get_assoc/3 to extract `handle` and `uri` -- mirroring the
     `insert_record_args/9` decoding path that runs in production
     immediately before the failing query.

  3. Opens a single Postgres connection via postgresql-prolog's
     extended protocol (same code path as the worker), then for
     each post in each round fires the production count(*) probe
     `SELECT count(*) FROM weaving_status WHERE ust_hash = $1`
     with `sha256(Handle || "|" || URI)` as the bind value.

Status against the production crash
-----------------------------------

  * 50 bare count(*) calls, no JSON parse first: SURVIVES.
  * Multiple rounds of (load_feed_posts + walk-to-assoc + 15x
    pg_query) over the frozen fixture: SURVIVES.
  * Production worker (HTTP fetch -> json_chars-DCG parse ->
    pairs_to_assoc -> maplist -> pg_query): SIGSEGV after the
    1st or 2nd pg_query on page 2.

So the fixture-driven path is not enough to trip it; the trigger
needs the live http_open stream / json_chars-DCG parse of the raw
chars to be in scope when pg_query runs. Wiring an HTTP fetch
into this file (against a public JSON endpoint that doesn't need
Bluesky auth, e.g. https://api.github.com/...) is the next step
toward a self-contained upstream-postable reproducer; today we
intentionally keep this file fixture-only and DB-only so it can
sit in the repo's test tree without depending on outbound
network.

Backtrace observed in production
--------------------------------

    EXC_BAD_ACCESS (code=1, address=0x301c7)
    scryer_prolog::machine::unify::Unifier::unify_constant + 84
    scryer_prolog::machine::unify::Unifier::unify_internal + 1164
    scryer_prolog::machine::dispatch::Machine::dispatch_loop + 127340

The address `0x301c7` is not a valid arena pointer -- it looks
like an ArenaHeaderTag has been read out of a freed or never-
initialised cell. unify_constant dereferences it and segfaults.

Outcomes when run
-----------------

  - All rounds processed cleanly -> [OK]. The fixture-driven
                                    path alone is insufficient
                                    today (see above).
  - Process killed by SIGSEGV    -> reproduced. The enclosing
                                    runner sees exit_code=139
                                    (128 + SIGSEGV) which fun.sh's
                                    `classify_worker_exit` labels
                                    as worker_crashed_by_signal=11.

Env vars required (same set fun.sh's configure exports):
  DATABASE_HOST, DATABASE_PORT, DATABASE_USERNAME,
  DATABASE_PASSWORD, DATABASE_DB_NAME.
*/

fixture_path("./tests/fixtures/segv-investigation/last-feed-pairs.pl").

env_or_die(NameChars) :-
    (   getenv(NameChars, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [NameChars]),
        halt(2)
    ).

env_chars(NameChars, ValueChars) :-
    getenv(NameChars, ValueChars).

env_number(NameChars, Number) :-
    getenv(NameChars, ValueChars),
    number_chars(Number, ValueChars).

open_connection(Conn) :-
    env_chars("DATABASE_USERNAME", User),
    env_chars("DATABASE_PASSWORD", Pass),
    env_chars("DATABASE_HOST", Host),
    env_number("DATABASE_PORT", Port),
    env_chars("DATABASE_DB_NAME", DB),
    connect(User, Pass, Host, Port, DB, Conn).

load_feed_posts(Posts) :-
    fixture_path(Path),
    open(Path, read, Stream),
    catch(
        read_term(Stream, Term, []),
        E,
        (close(Stream), throw(E))
    ),
    close(Stream),
    Term = last_feed_pairs(ResponsePairs),
    pairs_to_assoc(ResponsePairs, FeedAssoc),
    get_assoc(feed, FeedAssoc, Posts).

extract_handle_uri(WrappedPost, Handle, URI) :-
    wrapped_pairs_to_assoc(WrappedPost, PostAssoc),
    get_assoc(post, PostAssoc, FeedPost),
    get_assoc(author, FeedPost, Author),
    get_assoc(handle, Author, Handle),
    get_assoc(uri, FeedPost, URI).

% sha256(Handle || "|" || URI) -- identical to client:hash/2 so the
% reproducer hits the same `ust_hash` lookups the worker does.
post_hash(Handle, URI, Hash) :-
    append([Handle, "|", URI], UniqueIdentifier),
    crypto_data_hash(UniqueIdentifier, Hash, [algorithm(sha256)]).

count_sql([
    'S','E','L','E','C','T',' ',
    'c','o','u','n','t','(','*',')',' ',
    'F','R','O','M',' ',
    'p','u','b','l','i','c','.','w','e','a','v','i','n','g','_','s','t','a','t','u','s',' ',
    'W','H','E','R','E',' ',
    'u','s','t','_','h','a','s','h',' ','=',' ','$','1'
]).

run_iteration(Conn, SQL, Index-Post) :-
    extract_handle_uri(Post, Handle, URI),
    post_hash(Handle, URI, Hash),
    format("[..] iter=~w handle=~s uri=~s hash=~s~n",
           [Index, Handle, URI, Hash]),
    query(Conn, SQL, [Hash], Result),
    summarize(Index, Result).

summarize(Index, data([])) :-
    format("[ok] iter=~w empty data~n", [Index]).
summarize(Index, data([[Cell|_]|_])) :-
    format("[ok] iter=~w count=~s~n", [Index, Cell]).
summarize(Index, error(Err)) :-
    format("[!!] iter=~w pg_error=~q~n", [Index, Err]),
    halt(3).

zip_with_indices([], _, []).
zip_with_indices([X|Xs], I, [I-X|Rest]) :-
    I1 is I + 1,
    zip_with_indices(Xs, I1, Rest).

% Each round = one full load_feed_posts + pairs_to_assoc walk
% (so the JSON-DCG / nested-assoc arena state matches a fresh
% page parse) followed by one pg_query per post. The production
% trigger needs at least two such rounds because the worker
% only crashes on the 2nd or 3rd extended-protocol query AFTER
% the deep-term parse, never on the first.
how_many_rounds(5).

run_round(Conn, SQL, RoundIndex) :-
    load_feed_posts(Posts),
    zip_with_indices(Posts, 1, Indexed),
    format("[..] round=~w starting~n", [RoundIndex]),
    maplist(run_iteration(Conn, SQL), Indexed),
    format("[..] round=~w done~n", [RoundIndex]).

run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),
    open_connection(Conn),
    count_sql(SQL),
    how_many_rounds(R),
    numlist(1, R, RoundIndices),
    catch(
        ( maplist(run_round(Conn, SQL), RoundIndices),
          format("[OK] survived all ~w rounds -- crash needs more state~n", [R]),
          halt(0)
        ),
        Err,
        ( format("[!!] threw before SIGSEGV: ~q~n", [Err]),
          halt(1) )
    ).

:- initialization(run_test).
