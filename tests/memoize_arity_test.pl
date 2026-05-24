:- module(memoize_arity_test, [main/0]).

:- use_module(library(lists)).

:- use_module('../src/assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module('../src/app/bsky/feed/getAuthorFeed', [
    app__bsky__feed__getAuthorFeed/2
]).

/*
Regression test for the production crash:

    error(representation_error(max_arity), assertz/1)
    % Warning: initialization failed for:
    %   app__bsky__feed__getAuthorFeed("...", Prop)
    Process 1237358 dead!

Scryer Prolog caps compound arity at 255 when compiling a clause for
a dynamic predicate. The legacy memoize wrapper for
`app__bsky__feed__getAuthorFeed/2` did

    assertz(app__bsky__feed__getAuthorFeed_memoized(Params, Props))

where `Props` was the raw JSON-DCG parse of the feed response.
Real responses (15+ posts, each with multiple nested objects)
push the inner pair list past 254 elements, so the assertz
throws and the worker dies.

`memoize_arity:scryer_assertz_300_pairs_throws_max_arity` documents
the runtime limit that breaks assertz;
`memoize_arity:getAuthorFeed_has_no_dynamic_memo_table` verifies the
public predicate no longer goes through any dynamic-clause storage
and therefore can't hit the limit.
*/

:- dynamic(legacy_memo/2).

% Build a JSON-DCG-shaped pair list of length N.
build_pairs(0, []).
build_pairs(N, [string("k")-string("v")|Rest]) :-
    N > 0,
    N1 is N - 1,
    build_pairs(N1, Rest).

test(Spec) :-
    Spec = 'memoize_arity:scryer_assertz_300_pairs_throws_max_arity',
    build_pairs(300, BigPairs),
    Params = actor("a")-cursor("c"),
    assert(
        Spec,
        catch(
            assertz(legacy_memo(Params, BigPairs)),
            error(representation_error(max_arity), assertz/1),
            Outcome = thrown_max_arity
        ),
        thrown_max_arity,
        Outcome
    ).
test(Spec) :-
    Spec = 'memoize_arity:getAuthorFeed_has_no_dynamic_memo_table',
    assert(
        Spec,
        ( (   current_predicate(getAuthorFeed:app__bsky__feed__getAuthorFeed_memoized/2)
          ->  Outcome = legacy_dynamic_memo_still_defined
          ;   Outcome = no_dynamic_memo
          )
        ),
        no_dynamic_memo,
        Outcome
    ).

main :-
    Specs = [
        'memoize_arity:scryer_assertz_300_pairs_throws_max_arity',
        'memoize_arity:getAuthorFeed_has_no_dynamic_memo_table'
    ],
    run_test_suite(memoize_arity_test, Specs).

:- initialization(main).
