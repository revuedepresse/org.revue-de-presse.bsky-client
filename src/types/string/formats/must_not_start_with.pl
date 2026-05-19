:- module(must_not_start_with, [must_not_start_with/2]).

/**
Assert that a char list does not start with a given character.

Throws `error_must_not_start_with/1` when the first character
matches. Used by handle validation (no leading hyphen, no
leading period) and as the building block for
[[must_not_end_with]].
*/

:- use_module(library(si)).
:- use_module(must_be_ground, [must_be_ground/1]).

%% must_not_start_with(+Subject, +Char)
%
% Succeed iff the first element of `Subject` differs from
% `Char`; throw `error_must_not_start_with/1` otherwise.
must_not_start_with(Subject, Char) :-
    must_be_ground(Subject),
    must_be_ground(Char),
    Subject = [FirstChar|_],
    assert_first_differs(FirstChar, Char).

assert_first_differs(FirstChar, Char) :- dif_si(FirstChar, Char).
assert_first_differs(FirstChar, Char) :-
    \+ dif_si(FirstChar, Char),
    throw(error_must_not_start_with(Char)).
