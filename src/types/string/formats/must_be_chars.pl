:- module(must_be_chars, [must_be_chars/1]).

:- use_module(library(si)).
:- use_module(must_be_ground, [must_be_ground/1]).

% must_be_chars(+Subject).
must_be_chars(Subject) :-
    must_be_ground(Subject),
    (   \+ chars_si(Subject)
    ->  throw(type_error('Subject must be chars.'))
    ;   true ).