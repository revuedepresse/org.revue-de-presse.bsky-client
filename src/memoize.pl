:- module(memoize, [
    memoize_goal/2,
    memoized_goal/2
]).

/**
Generic memoization for any prolog goal.

`memoize_goal/2` consults the dynamic, multifile
`memoized_goal/2` fact for the goal's functor and arguments,
calls the goal on a miss, and asserts the result. Goals that
fail rather than succeed throw an explicit error so callers
notice they handed memoization the wrong kind of predicate.
*/

:- use_module(library(freeze)).

:- use_module(logger, [log_debug/1]).

%% memoized_goal(?GoalFunctor, ?Args)
%
% Dynamic multifile cache of memoized goal results. Other
% modules may assert into it directly to seed the cache.
:- dynamic(memoized_goal/2).
:- multifile(memoized_goal/2).

%% memoize_goal(:Goal, +Args)
%
% Return cached `Args` from a prior call of `Goal` if known,
% otherwise call `Goal` and assert the result keyed by `Goal`'s
% functor. Throws `error_cannot_memoize_failing_goal` if `Goal`
% fails on a cache miss.
memoize_goal(Goal, Args) :-
    Goal =.. [Term|_GoalArgs],
    once(memoize_or_compute(Term, Goal, Args)).

memoize_or_compute(Term, _, Args) :-
    memoized_goal(Term, Args),
    log_debug(["memoized args: ", Args]).
memoize_or_compute(Term, Goal, Args) :-
    \+ memoized_goal(Term, Args),
    call(Goal),
    assertz(memoized_goal(Term, Args)).
memoize_or_compute(Term, Goal, Args) :-
    \+ memoized_goal(Term, Args),
    \+ call(Goal),
    throw(error_cannot_memoize_failing_goal).
