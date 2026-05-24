:- module(sigsegv_min_repro, [run/0]).

:- use_module(library(assoc)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(serialization/json)).

:- use_module('../../src/stream', [read_stream/2]).
:- use_module('../../src/serialization', [pairs_to_assoc/2]).
:- use_module('../../src/infrastructure/repository/repository_status', [
    exists_by_uri_t/3
]).

/*
The minimal reproducer for the patched-VM SIGSEGV in
unify_constant. See doc/SIGSEGV-REPRODUCER.md for the full
investigation report.

Crash signature on stock scryer-prolog v0.10.0-186-g29839848:

    #0  scryer_prolog::types::UntypedArenaPtr::get_tag
    #1  scryer_prolog::machine::unify::Unifier::unify_constant
    #2  scryer_prolog::machine::unify::Unifier::unify_internal
    #3  scryer_prolog::machine::Machine::dispatch_loop

Setup
-----
    git fetch origin && git checkout reproduce-sigsegv-issue
    make scryer-prolog-build      (or docker-segv-reproducer-build)
    make compose-up               (brings up the test PG)
    PGPASSWORD=test psql -h 127.0.0.1 -p 55432 -U test \
        -d revue_de_presse_test \
        -c "TRUNCATE TABLE public.weaving_status RESTART IDENTITY CASCADE;"

Run
---
    SOURCE=none \
    DATABASE_HOST=127.0.0.1 DATABASE_PORT=55432 \
    DATABASE_USERNAME=test DATABASE_PASSWORD=test \
    DATABASE_DB_NAME=revue_de_presse_test \
    PATH=./deps/scryer-prolog/target/release:$PATH \
        scryer-prolog ./tests/pg/sigsegv_min_repro.pl -g run

Expected on stock scryer
------------------------
    [..] SOURCE=none (no read_stream at all)
    [..] Q0: exists_by_uri_t(min.example, at://min/post/0, T)
    [..] Q0 returned T=false
    [..] Q1: repository_status:insert/3
    Segmentation fault (core dumped)

Expected with the recovery patch in this branch
-----------------------------------------------
    [..] Q1: repository_status:insert/3
    [KO] repository_status:insert threw: pg_query_silently_failed(...)
    (no SIGSEGV, no coredump; exit 1)

What this proves
----------------
The two `repository_status` predicates -- exists_by_uri_t/3 and
insert/3 -- in sequence on the same Postgres connection deterministically
trigger the bug. No HTTP, no JSON, no captured production data,
no specific post content. The same two predicates replaced with
hand-written equivalents that exercise the same primitives
(`crypto_data_hash`, `write_term_to_chars`, `chars_utf8bytes`,
`chars_base64`, `if_/3`, project's `pg_query/3` with cached
connection) -- all of those combinations SURVIVE. See
sigsegv_pgquery_repro.pl for the full bisect harness.

Inputs (env)
------------
    SOURCE        "none" (default in this minimal form) -- skip
                  any read_stream entirely. "http" or "file"
                  variants are kept as counter-example knobs
                  showing the bug doesn't need them.
    POST_INDEX    unused in SOURCE=none mode (kept for the
                  http/file modes' compatibility with the wider
                  reproducer suite)
    DATABASE_*    same set as fun.sh's configure
*/

:- use_module(library(http/http_open)).

env_or_die(Name) :-
    (   getenv(Name, _) -> true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

env_chars_default(Name, Default, Value) :-
    (   getenv(Name, V) -> Value = V ; Value = Default ).

env_bool(Name) :-
    getenv(Name, Value),
    ( Value = "1" ; Value = "true" ),
    !.

default_url("http://127.0.0.1:8080/feed.json").

run :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    env_chars_default("SOURCE", "http", Source),
    (   Source == "none"
    ->  format("[..] SOURCE=none (no read_stream at all)~n", [])
    ;   Source == "file"
    ->  env_chars_default("FEED_FILE", "/tmp/minimal-feed.json", Path),
        format("[..] SOURCE=file open(~s)~n", [Path]),
        open(Path, read, Stream),
        read_stream(Stream, BodyChars),
        length(BodyChars, BodyLen),
        format("[..] body chars=~w~n", [BodyLen])
    ;   default_url(DefaultUrl),
        env_chars_default("FEED_URL", DefaultUrl, URL),
        format("[..] SOURCE=http http_open(~s)~n", [URL]),
        http_open(URL, Stream, []),
        read_stream(Stream, BodyChars),
        length(BodyChars, BodyLen),
        format("[..] body chars=~w~n", [BodyLen])
    ),

    % Hardcoded synthetic field values. The bug doesn't need
    % captured production data -- proven by the data-bisect (see
    % sigsegv_databisect_repro.pl + the SYNTH_* knobs).
    Handle = "min.example",
    URI = "at://min/post/0",
    Text = "hello",
    DisplayName = "M",
    AuthorAvatar = "https://x/a",
    CreatedAt = "2024-01-01T00:00:00.000Z",
    empty_assoc(Payload),

    format("[..] Q0: exists_by_uri_t(~s, ~s, T)~n", [Handle, URI]),
    exists_by_uri_t(Handle, URI, T0),
    format("[..] Q0 returned T=~q~n", [T0]),

    format("[..] Q1: repository_status:insert/3~n", []),
    catch(
        ( repository_status:insert(
              row(DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt),
              StatusResult, RecordId
          ),
          format("[OK] survived: Q1 returned ~q record_id=~q~n", [StatusResult, RecordId]),
          halt(0)
        ),
        Err,
        ( format("[KO] repository_status:insert threw: ~q~n", [Err]),
          halt(1) )
    ).

:- initialization(run).
