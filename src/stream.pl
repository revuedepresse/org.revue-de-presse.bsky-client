:- module(stream, [
    read_stream/2,
    read_stream/3,
    writeln/1,
    writeln/2
]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(si)).

% read_stream(+Stream, -Out).
read_stream(Stream, Out) :-
    get_n_chars(Stream, _, Out),
    close(Stream).

% read_stream(+Stream, +In, -Out).
read_stream(Stream, _In, Out) :-
    read_stream(Stream, Out).

writeln(_Term) :- !.

writeln_(_Term, false) :- !.
writeln_(Term, true) :-
    write_term(Term, [double_quotes(true)]),
    nl.
writeln_(Term, true) :-
    \+ chars_si(Term),
    write(Term),
    nl.

writeln(Term, Cond) :-
    if_(
        Cond = true,
        once(writeln_(Term, true)),
        true
    ).