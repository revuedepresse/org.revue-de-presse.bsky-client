:- module(must_not_start_with, [must_not_start_with/2]).

:- use_module(library(si)).
:- use_module(must_be_ground, [must_be_ground/1]).

% must_not_start_with(+Subject, +Char).
must_not_start_with(Subject, Char) :-
    must_be_ground(Subject),
    must_be_ground(Char),
    Subject = [FirstChar|_],
    assert_first_differs(FirstChar, Char).

assert_first_differs(FirstChar, Char) :- dif_si(FirstChar, Char).
assert_first_differs(FirstChar, Char) :-
    \+ dif_si(FirstChar, Char),
    throw(error_must_not_start_with(Char)).
