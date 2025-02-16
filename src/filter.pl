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

    (   call(Goal)
    ->  append([In, [Head]], NextIn)
    ;   NextIn = In ),
    include(PartialGoal, Tail, NextIn, Out).

include(PartialGoal, L, Out) :-
    list_si(L),
    include(PartialGoal, L, [], Out).