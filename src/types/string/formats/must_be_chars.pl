:- module(must_be_chars, [must_be_chars/1]).

/**
Type guard: assert that a subject is a ground char list.

Used at the top of string-format predicates that need to walk a
char list with `char_code/2` or `length/2`. Throws
`type_error('Subject must be chars.')` on non-chars rather than
failing silently, so misuse fails loud.
*/

:- use_module(library(si)).
:- use_module(must_be_ground, [must_be_ground/1]).

%% must_be_chars(+Subject)
%
% Succeed iff `Subject` is ground and satisfies `chars_si/1`;
% otherwise throw a `type_error/1`.
must_be_chars(Subject) :-
    must_be_ground(Subject),
    once(assert_chars(Subject)).

assert_chars(Subject) :- chars_si(Subject).
assert_chars(Subject) :-
    \+ chars_si(Subject),
    throw(type_error('Subject must be chars.')).