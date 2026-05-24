:- module(reconstruct_feed_json, [run/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/stream', [read_stream/2]).

/*
Reconstruct the raw JSON body from a captured last-feed-body.txt.

The capture writer at getAuthorFeed.pl:170 uses plain `write/1` on
the BodyChars list (a list of single-character atoms representing
the HTTP response body). scryer's write/1 emits a list literal like

    [{,",f,e,e,d,",:,[,{,",p,o,s,t,...]

i.e. for an N-character body the inner string between [ and ] is
exactly 2N-1 bytes long: every even-indexed byte (0-based) is one
JSON character and every odd-indexed byte is the list separator
comma the printer inserted. So the JSON text is the even-indexed
slice of the inner string. Even commas inside the JSON are
correctly preserved: each is at an even index, between two odd
separator commas.

This script reads the input file and emits the reconstructed JSON
to disk so downstream tests can:
  - parse it with phrase(json_chars(pairs(R)), BodyChars) -- the
    same DCG the production worker uses, exercising the same
    arena allocation path that read_term/2 sidesteps;
  - serve it from a local HTTP server (e.g. python -m http.server)
    to drive the full http_open path in a test reproducer.

Usage
-----
    INPUT=path/to/last-feed-body.txt \
    OUTPUT=path/to/feed-body.json \
    scryer-prolog ./tests/pg/reconstruct_feed_json.pl -g run

If OUTPUT is omitted it defaults to <INPUT-without-.txt>.json
sibling.
*/

env_required(Name, Value) :-
    (   getenv(Name, Value) -> true
    ;   format("[KO] missing env var ~s~n", [Name]),
        halt(2)
    ).

env_optional(Name, Default, Value) :-
    (   getenv(Name, V) -> Value = V ; Value = Default ).

default_output_path(InputChars, OutputChars) :-
    (   append(Prefix, ".txt", InputChars)
    ->  append(Prefix, ".json", OutputChars)
    ;   append(InputChars, ".json", OutputChars)
    ).

% Decode the list-literal Prolog format emitted by write/1 on a
% chars list into the underlying chars. Strips the bracketing
% [ and ] then takes every other element (the ones at even
% 0-based positions in the inner string).
decode_list_literal([], []).
decode_list_literal(['['|Rest], Chars) :-
    drop_last_bracket(Rest, Inner),
    even_indexed(Inner, Chars).

drop_last_bracket([C], []) :-
    (   C = ']' -> true
    ;   format("[KO] last char is ~q, expected ]~n", [C]), halt(3)
    ).
drop_last_bracket([C|Rest], [C|Out]) :-
    Rest = [_|_],
    drop_last_bracket(Rest, Out).

even_indexed([], []).
even_indexed([X], [X]).
even_indexed([X, _Sep | Rest], [X | Out]) :-
    even_indexed(Rest, Out).

write_chars(_, []).
write_chars(Stream, [C|Cs]) :-
    put_char(Stream, C),
    write_chars(Stream, Cs).

run :-
    env_required("INPUT", InputPath),
    default_output_path(InputPath, DefaultOutputPath),
    env_optional("OUTPUT", DefaultOutputPath, OutputPath),

    format("[..] input=~s~n",  [InputPath]),
    format("[..] output=~s~n", [OutputPath]),

    open(InputPath, read, In),
    % read_stream/2 reads to EOF and closes the stream itself
    % (src/stream.pl:26-28), so no explicit close here.
    read_stream(In, RawChars),

    length(RawChars, RawLen),
    format("[..] raw input length=~w~n", [RawLen]),

    decode_list_literal(RawChars, JsonChars),
    length(JsonChars, JsonLen),
    format("[..] decoded JSON length=~w~n", [JsonLen]),

    open(OutputPath, write, Out),
    catch(
        write_chars(Out, JsonChars),
        E2,
        (close(Out), throw(E2))
    ),
    close(Out),
    format("[OK] wrote ~w chars to ~s~n", [JsonLen, OutputPath]).

:- initialization(run).
