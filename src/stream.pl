:- module(stream, [
    read_stream/2,
    read_stream/3,
    writeln/1,
    writeln/2
]).

/**
Stream and `writeln/1` helpers.

`read_stream/2` reads a stream to EOF as chars and closes it.
`writeln/2` is a conditional `writeln` whose second argument
gates printing — used for cheap log-on-flag patterns where the
caller doesn't want to thread a logger through every step.
*/

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(si)).

%% read_stream(+Stream, -Out)
%
% Read `Stream` to EOF and unify `Out` with the resulting char
% list. `Stream` is closed before returning.
read_stream(Stream, Out) :-
    get_n_chars(Stream, _, Out),
    close(Stream).

%% read_stream(+Stream, +In, -Out)
%
% Variant of `read_stream/2` that ignores `In` — kept for call
% sites that thread state through a fold.
read_stream(Stream, _In, Out) :-
    read_stream(Stream, Out).

%% writeln(+L, +Cond)
%
% List-prefixed conditional writeln: when `Cond = true`, print
% `% Key Args ...` followed by a newline.
writeln([Key|Args], Cond) :-
    once(writeln('%':[Key|Args], Cond)).

%% writeln(+Term, +Cond)
%
% Print `Term` followed by a newline when `Cond = true`. Used
% by the XRPC clients to log endpoint / response details only
% under verbose modes.
writeln(Term, Cond) :-
    \+ list_si(Term),
    if_(
        Cond = true,
        once(writeln_(Term, true)),
        true
    ).

    % writeln_(+Term, +Cond).
    writeln_(_Term, false) :- !.
    writeln_(Term, true) :-
        write_term(Term, [double_quotes(true)]),
        nl.
    writeln_(Term, true) :-
        \+ chars_si(Term),
        write(Term),
        nl.

    %% writeln(+Term)
    %
    % Unconditional no-op writeln kept for arity-1 call sites
    % that don't need the conditional gate.
    writeln(_Term) :- !.
