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

    memoized_goal(Term, Args)
    ->  once((log_debug(["memoized args: ", Args]), true))
    ;   (   call(Goal)
        ->  assertz(memoized_goal(Term, Args))
        ;   throw(error_cannot_memoize_failing_goal) ).
