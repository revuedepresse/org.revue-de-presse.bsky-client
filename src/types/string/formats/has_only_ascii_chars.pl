:- module(has_only_ascii_chars, [
    has_only_ascii_chars/1,
    must_be_ascii_char/1
]).

/**
ASCII-only char list checks.

`has_only_ascii_chars/1` walks every character with
`must_be_ascii_char/1`, which throws a `type_error/1` on the
first non-ASCII character. Used by handle and DID validators.

Written in the reified style src/link_shortener.pl sets out:
being ASCII is a condition whose last argument is a truth
value, and `if_/3` picks the branch. No negation as failure,
so the condition can be asked rather than only tried.
*/

:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(reif)).
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
    if_(ascii_char_t(Char),
        true,
        throw(type_error('Subject must start with an ascii char.'))).

%% ascii_char_t(+Char, -Truth)
%
% ASCII is the lower half of the code space, so the character
% is read as its code rather than compared against character
% literals -- which is also the only shape that works for the
% operator atoms (`-`, `,`, `<`, `/`) a handle may carry.
ascii_char_t(Char, Truth) :-
    char_code(Char, Code),
    code_in_t(0, 0x7F, Code, Truth).

%% code_in_t(+Low, +High, +Value, -Truth)
%
% `true` when `Low =< Value =< High`.
code_in_t(Low, High, Value, Truth) :-
    zcompare(BelowOrder, Low, Value),
    zcompare(AboveOrder, Value, High),
    range_truth(BelowOrder, AboveOrder, Truth).

range_truth(<, <, true).
range_truth(<, =, true).
range_truth(<, >, false).
range_truth(=, <, true).
range_truth(=, =, true).
range_truth(=, >, false).
range_truth(>, <, false).
range_truth(>, =, false).
range_truth(>, >, false).
