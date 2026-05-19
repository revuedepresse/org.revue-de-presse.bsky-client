:- module(memoize, [
    memoize_goal/2,
    memoized_goal/2
]).

:- use_module(library(freeze)).

:- use_module(logger, [log_debug/1]).

:- dynamic(memoized_goal/2).
:- multifile(memoized_goal/2).

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
