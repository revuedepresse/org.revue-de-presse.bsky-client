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

%% read_stream(+Stream, -Out).
%
% Unifying Out with chars read until EOF is found,
% before closing Stream and
% after reading all chars available.
read_stream(Stream, Out) :-
    get_n_chars(Stream, _, Out),
    close(Stream).

%% read_stream(+Stream, +In, -Out).
read_stream(Stream, _In, Out) :-
    read_stream(Stream, Out).

%% writeln(+L, +Cond).
writeln([Key|Args], Cond) :-
    once(writeln('%':[Key|Args], Cond)).

%% writeln(+Term, +Cond).
writeln(Term, Cond) :-
    \+ list_si(Term),
    if_(
        Cond = true,
        once(writeln_(Term, true)),
        true
    ).

    %% writeln_(+Term, +Cond).
    writeln_(_Term, false) :- !.
    writeln_(Term, true) :-
        write_term(Term, [double_quotes(true)]),
        nl.
    writeln_(Term, true) :-
        \+ chars_si(Term),
        write(Term),
        nl.

    writeln(_Term) :- !.
