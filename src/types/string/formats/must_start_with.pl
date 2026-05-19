:- module(must_start_with, [must_start_with/2]).

/**
Assert that a char list starts with a given substring.

Throws `error_must_start_with/1` when the prefix doesn't
match. Used by DID validation (the literal `"did:"` prefix
check) and at-identifier dispatch.
*/

:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(must_be_chars, [must_be_chars/1]).

assert_starts_with(Substring, Substring).
assert_starts_with(SubStr, Substring) :-
    SubStr \= Substring,
    throw(error_must_start_with(Substring)).

%% must_start_with(+Subject, +Substring)
%
% Succeed iff the first `length(Substring)` chars of `Subject`
% equal `Substring`; throw `error_must_start_with/1` otherwise.
must_start_with(Subject, Substring) :-
    must_be_chars(Subject),
    must_be_chars(Substring),
    length(Subject, N),
    length(Substring, SubstringLength),

    N #>= SubstringLength,

    atom_chars(SubjectAtom, Subject),
    sub_atom(SubjectAtom, 0, SubstringLength, _, SubAtom),
    atom_chars(SubAtom, SubStr),

    assert_starts_with(SubStr, Substring).
