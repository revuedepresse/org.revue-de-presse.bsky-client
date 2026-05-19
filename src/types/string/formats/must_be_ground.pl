:- module(must_be_ground, [must_be_ground/1]).

/**
Type guard: assert that a subject is fully ground.

The string-format validators need a ground value before
performing any positional or character-level inspection. This
predicate throws an `instantiation_error/1` on partial terms
so we don't silently accept variables.
*/

%% must_be_ground(+Subject)
%
% Succeed iff `Subject` is fully ground; throw an
% `instantiation_error/1` otherwise.
must_be_ground(Subject) :-
    once(must_be_ground_or_throw(Subject)).

must_be_ground_or_throw(Subject) :- ground(Subject).
must_be_ground_or_throw(Subject) :-
    \+ ground(Subject),
    throw(instantiation_error('Subject must be ground.')).
