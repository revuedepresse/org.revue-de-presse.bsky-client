:- module(has_only_ascii_chars, [
    has_only_ascii_chars/1,
    must_be_ascii_char/1
]).

/**
ASCII-only char list checks.

`has_only_ascii_chars/1` walks every character with
`must_be_ascii_char/1`, which throws a `type_error/1` on the
first non-ASCII character. Used by handle and DID validators.
*/

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(si)).

%% has_only_ascii_chars(+Subject)
%
% Succeed iff every character of `Subject` is ASCII; throw via
% `must_be_ascii_char/1` on the first non-ASCII character.
has_only_ascii_chars(Subject) :-
    maplist(must_be_ascii_char, Subject).

%% must_be_ascii_char(+Char)
%
% Succeed iff `Char` is an ASCII character; throw
% `type_error('Subject must start with an ascii char.')`
% otherwise. The overall handle must contain only ASCII
% characters.
must_be_ascii_char(Char) :-
    atom_si(Char),
    assert_ascii(Char).

assert_ascii(Char) :- char_type(Char, ascii).
assert_ascii(Char) :-
    \+ char_type(Char, ascii),
    throw(type_error('Subject must start with an ascii char.')).