:- module(reconstruct_feed_json, [run/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(serialization/json)).

/*
Reconstruct the raw JSON body that the worker had in flight at
SIGSEGV time, into a file that can be read back and parsed via
`phrase(json_chars(pairs(_)), Chars)` -- i.e. the production
code path at getAuthorFeed.pl:117.

Approach
--------
Read the captured last-feed-pairs.pl (a canonical Prolog term
written via write_canonical/1, so read_term/2 reads it back
losslessly). Then run scryer's bidirectional `json_chars//1`
DCG (deps/scryer-prolog/src/lib/serialization/json.pl) in
generator mode against the parsed pairs to emit JSON chars.
Write the chars to disk as plain text.

Why not decode last-feed-body.txt directly?
The capture writer (getAuthorFeed.pl:170) emits the body via
plain `write/1`, which produces `[c0,c1,c2,...]` notation. Naive
byte-level decoding of that notation gets fragile for any char
that the printer escapes or for multi-byte UTF-8 -- the JSON
that comes back parses to "{" then fails. Generating fresh JSON
from the already-parsed pairs sidesteps the problem entirely
and produces the exact JSON shape `json_chars//1` accepts on the
way back in.

Usage
-----
    INPUT=path/to/last-feed-pairs.pl \
    OUTPUT=path/to/feed-body.json \
    scryer-prolog ./tests/pg/reconstruct_feed_json.pl -g run
*/

env_required(Name, Value) :-
    (   getenv(Name, Value) -> true
    ;   format("[KO] missing env var ~s~n", [Name]),
        halt(2)
    ).

env_optional(Name, Default, Value) :-
    (   getenv(Name, V) -> Value = V ; Value = Default ).

default_output_path(InputChars, OutputChars) :-
    (   append(Prefix, ".pl", InputChars)
    ->  append(Prefix, ".json", OutputChars)
    ;   append(InputChars, ".json", OutputChars)
    ).

load_response_pairs(Pairs) :-
    env_required("INPUT", InputPath),
    format("[..] input=~s~n", [InputPath]),
    open(InputPath, read, In),
    catch(
        read_term(In, Term, []),
        E,
        (close(In), throw(E))
    ),
    close(In),
    Term = last_feed_pairs(Pairs).

write_chars(_, []).
write_chars(Stream, [C|Cs]) :-
    put_char(Stream, C),
    write_chars(Stream, Cs).

run :-
    load_response_pairs(Pairs),
    env_required("INPUT", InputPath),
    default_output_path(InputPath, DefaultOutputPath),
    env_optional("OUTPUT", DefaultOutputPath, OutputPath),
    format("[..] output=~s~n", [OutputPath]),

    format("[..] generating JSON via phrase(json_chars(pairs(_)), Chars) in generator mode~n", []),
    phrase(json_chars(pairs(Pairs)), JsonChars),
    length(JsonChars, JsonLen),
    format("[..] generated JSON length=~w chars~n", [JsonLen]),

    open(OutputPath, write, Out),
    catch(
        write_chars(Out, JsonChars),
        E,
        (close(Out), throw(E))
    ),
    close(Out),
    format("[OK] wrote ~w chars to ~s~n", [JsonLen, OutputPath]).

:- initialization(run).
