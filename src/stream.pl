:- module(stream, [
    read_stream/3,
    writeln/1
]).

:- use_module(library(si)).

% read_stream(+Stream, +In, -Out).
read_stream(Stream, In, Out) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Out = In
    ;   read_stream(Stream, In, ReadOut),
        Out = [Char|ReadOut] ).

writeln(X) :-
    (   atomic_si(X)
    ->  X = AtomicX
    ;   atom_chars(AtomicX, X) ),
    write(AtomicX), nl.