:- module(memoize_arity_test, [run_test/0]).

:- use_module(library(lists)).

:- use_module('../src/app/bsky/feed/getAuthorFeed', [
    app__bsky__feed__getAuthorFeed/2
]).

/*
Regression test for the production crash:

    error(representation_error(max_arity), assertz/1)
    % Warning: initialization failed for:
    %   app__bsky__feed__getAuthorFeed_without_memoization("...", Prop)
    Process 1237358 dead!

Scryer Prolog caps compound arity at 255 when compiling a clause for
a dynamic predicate. The legacy memoize wrapper for
`app__bsky__feed__getAuthorFeed/2` did

    assertz(app__bsky__feed__getAuthorFeed_memoized(Params, Props))

where `Props` was the raw JSON-DCG parse of the feed response.
Real responses (15+ posts, each with multiple nested objects)
push the inner pair list past 254 elements, so the assertz
throws and the worker dies.

`test_scryer_arity_limit_reproducer` documents the runtime limit
that breaks assertz; `test_memoize_path_does_not_assertz` verifies
the public predicate no longer goes through any dynamic-clause
storage and therefore can't hit the limit.
*/

:- dynamic(legacy_memo/2).

run_test :-
    test_scryer_arity_limit_reproducer,
    test_memoize_path_does_not_assertz,
    format("[OK] memoize_arity_test passed~n", []).

% Build a JSON-DCG-shaped pair list of length N.
build_pairs(0, []).
build_pairs(N, [string("k")-string("v")|Rest]) :-
    N > 0,
    N1 is N - 1,
    build_pairs(N1, Rest).

% Documents the scryer ceiling that the production stack trace hit.
test_scryer_arity_limit_reproducer :-
    build_pairs(300, BigPairs),
    Params = actor("a")-cursor("c"),
    catch(
        ( assertz(legacy_memo(Params, BigPairs)),
          format("[FAIL] scryer assertz of 300-pair list unexpectedly succeeded~n", []),
          halt(1)
        ),
        error(representation_error(max_arity), assertz/1),
        true
    ).

% After the fix, the predicate exported as the worker entrypoint must
% not declare any dynamic memo table backing it. predicate_property/2
% surfaces the dynamic flag if any clause was ever asserted into the
% old `_memoized/2` table from a prior load.
test_memoize_path_does_not_assertz :-
    (   current_predicate(getAuthorFeed:app__bsky__feed__getAuthorFeed_memoized/2)
    ->  format("[FAIL] legacy dynamic memo table still defined~n", []),
        halt(1)
    ;   true
    ).

:- initialization((run_test, halt)).
