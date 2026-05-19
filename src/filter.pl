:- module(filter, [include/3]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(si)).

is_required(Param) :-
    get_assoc(required, Param, true).

include(_PartialGoal, [], FilteredList, FilteredList).
include(PartialGoal, [Head|Tail], In, Out) :-
    PartialGoal =.. [Functor|Args],
    append([Args, [Head]], Arguments),
    Goal =.. [Functor|Arguments],
    once(accumulate_if_match(Goal, Head, In, NextIn)),
    include(PartialGoal, Tail, NextIn, Out).

accumulate_if_match(Goal, Head, In, NextIn) :-
    call(Goal),
    append([In, [Head]], NextIn).
accumulate_if_match(Goal, _, In, In) :-
    \+ call(Goal).

include(PartialGoal, L, Out) :-
    list_si(L),
    include(PartialGoal, L, [], Out).