:- module(concurrent_segv_reproducer, [run_test/0]).

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
Concurrent variant of tests/pg/sigsegv_repeat_pg_query_reproducer.pl.

The single-process scaffold survives even on the unpatched VM (the
trigger needs the live http_open stream / JSON-DCG parse concurrent
with pg_query). On the *patched* binary
(deps/scryer-prolog/target/release/scryer-prolog,
v0.10.0-186-g29839848) the 1273 production captures under
var/tmp/segv-investigation-pull/ show 20 SIGSEGVs that the upstream
`unify_constant` guard at b02e0c47 does NOT catch.

That 20-crash batch came from one run of `get_authors_feeds` in
~deployer/.maintaining/org.revue-de-presse.bsky.sh, which launches
THREE scryer processes simultaneously under `cpulimit -l 30`. The
new dimension this reproducer adds is concurrency: the
orchestrator (run_concurrent_segv_reproducer.sh) spawns N parallel
invocations, each looping over a different captured
`last-feed-pairs.pl` fixture. cpulimit-induced SIGSTOP/SIGCONT
churn is a follow-on layer if pure concurrency alone is not enough.

Inputs (env)
------------
  FEED_PAIRS_FIXTURE  path to a captured last-feed-pairs.pl
  REPRODUCER_ROUNDS   integer, default 20
  REPRODUCER_TAG      short label for log lines, default "w?"
  DATABASE_HOST       same set as fun.sh's configure
  DATABASE_PORT
  DATABASE_USERNAME
  DATABASE_PASSWORD
  DATABASE_DB_NAME

Exit codes
----------
   0  survived all rounds
   1  threw a non-pg exception (and was caught)
   2  missing env var
   3  pg_query returned error(_) -- not a SIGSEGV, a real PG error
 139  SIGSEGV (caught by the orchestrator's `wait`)
*/

env_or_die(NameChars) :-
    (   getenv(NameChars, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [NameChars]),
        halt(2)
    ).

env_chars(NameChars, ValueChars) :- getenv(NameChars, ValueChars).

env_number(NameChars, Number) :-
    getenv(NameChars, ValueChars),
    number_chars(Number, ValueChars).

env_chars_default(NameChars, _, ValueChars) :-
    getenv(NameChars, ValueChars), !.
env_chars_default(_, Default, Default).

open_connection(Conn) :-
    env_chars("DATABASE_USERNAME", User),
    env_chars("DATABASE_PASSWORD", Pass),
    env_chars("DATABASE_HOST", Host),
    env_number("DATABASE_PORT", Port),
    env_chars("DATABASE_DB_NAME", DB),
    connect(User, Pass, Host, Port, DB, Conn).

load_feed_posts(Posts) :-
    env_chars("FEED_PAIRS_FIXTURE", Path),
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

run_iteration(Tag, Conn, SQL, Round-Iter-Post) :-
    extract_handle_uri(Post, Handle, URI),
    post_hash(Handle, URI, Hash),
    query(Conn, SQL, [Hash], Result),
    summarize(Tag, Round, Iter, Result).

summarize(_, _, _, data([])).
summarize(_, _, _, data([[_|_]|_])).
summarize(Tag, R, I, error(Err)) :-
    format("[!!] tag=~s round=~w iter=~w pg_error=~q~n", [Tag, R, I, Err]),
    halt(3).

zip_with_round_index([], _, []).
zip_with_round_index([X|Xs], R-I, [R-I-X|Rest]) :-
    I1 is I + 1,
    zip_with_round_index(Xs, R-I1, Rest).

run_round(Tag, Conn, SQL, RoundIndex) :-
    load_feed_posts(Posts),
    zip_with_round_index(Posts, RoundIndex-1, Indexed),
    maplist(run_iteration(Tag, Conn, SQL), Indexed).

how_many_rounds(N) :-
    (   getenv("REPRODUCER_ROUNDS", Chars)
    ->  number_chars(N, Chars)
    ;   N = 20
    ).

run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME","FEED_PAIRS_FIXTURE"]),
    env_chars_default("REPRODUCER_TAG", "w?", Tag),
    how_many_rounds(R),
    env_chars("FEED_PAIRS_FIXTURE", Path),
    format("[..] tag=~s fixture=~s rounds=~w starting~n", [Tag, Path, R]),
    open_connection(Conn),
    count_sql(SQL),
    numlist(1, R, RoundIndices),
    catch(
        ( maplist(run_round(Tag, Conn, SQL), RoundIndices),
          format("[OK] tag=~s survived all ~w rounds~n", [Tag, R]),
          halt(0)
        ),
        Err,
        ( format("[!!] tag=~s threw: ~q~n", [Tag, Err]),
          halt(1) )
    ).

:- initialization(run_test).
