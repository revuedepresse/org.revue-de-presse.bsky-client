:- module(has_only_ascii_chars, [
    has_only_ascii_chars/1,
    must_be_ascii_char/1
]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(si)).

% has_only_ascii_chars(+Subject).
has_only_ascii_chars(Subject) :-
    maplist(must_be_ascii_char, Subject).

% The overall handle must contain only ASCII characters.
%
% must_be_ascii_char(+Char).
must_be_ascii_char(Char) :-
    atom_si(Char),
    (   \+ char_type(Char, ascii)
    ->  throw(type_error('Subject must start with an ascii char.'))
    ;   true ).