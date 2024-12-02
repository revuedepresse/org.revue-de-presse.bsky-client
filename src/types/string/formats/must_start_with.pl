:- module(must_start_with, [must_start_with/2]).

:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(must_be_chars, [must_be_chars/1]).

% must_start_with(+Subject, +Substring).
must_start_with(Subject, Substring) :-
    must_be_chars(Subject),
    must_be_chars(Substring),
    length(Subject, N),
    length(Substring, SubstringLength),

    N #>= SubstringLength,

    atom_chars(SubjectAtom, Subject),
    sub_atom(SubjectAtom, 0, SubstringLength, _, SubAtom),
    atom_chars(SubAtom, SubStr),

    SubStr \= Substring
    ->  throw(error_must_start_with(Substring))
    ;   true.
