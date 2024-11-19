:- module(must_be_ground, [must_be_ground/1]).

% must_be_ground(+Subject).
must_be_ground(Subject) :-
    (   \+ ground(Subject)
    ->  throw(instantiation_error('Subject must be ground.'))
    ;   true ).
