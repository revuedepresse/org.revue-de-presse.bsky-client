:- module(filter, [include/3]).

/**
A variant of `include/3` over a partially-applied goal.

Standard `lists:include/3` calls a unary goal directly. This
version takes a partial goal whose arguments are extended with
the element under test, so a binary check like `is_required/2`
can be threaded through without manual lambda construction.
*/

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

%% include(:PartialGoal, +List, -Filtered)
%
% Unifies `Filtered` with the sublist of `List` for which
% calling `PartialGoal` with the element appended succeeds.
% `List` must be ground enough to satisfy `list_si/1`.
include(PartialGoal, L, Out) :-
    list_si(L),
    include(PartialGoal, L, [], Out).