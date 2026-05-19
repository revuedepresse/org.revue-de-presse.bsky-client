:- module(serialization, [
    beautify_json/2,
    by_key/3,
    char_code_at/2,
    keys/3,
    pairs_to_assoc/2,
    unwrap_pairs/2,
    to_json_chars/2,
    wrapped_pairs_to_assoc/2
]).

/**
JSON wire-format helpers.

Scryer's `serialization/json` DCG yields parse trees as tagged
pair lists (`string(K)-string(V)`, `string(K)-number(V)`,
`string(K)-pairs(...)`, etc). The helpers here unwrap that
representation into native prolog assocs keyed by atoms, plus a
few utilities for keyed lookup and shell-based JSON
pretty-printing through `jq`.
*/

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(pairs)).
:- use_module(library(reif)).
:- use_module(library(si)).

:- use_module(logger, [
    log_debug/1,
    log_info/1
]).
:- use_module(os_ext, [
    remove_temporary_file/1,
    temporary_file/2,
    temporary_file/3
]).
:- use_module(stream, [
    read_stream/2
]).

%% char_code_at(+Code, -Char)
%
% Argument-flipped `char_code/2` for `maplist` convenience.
char_code_at(Code, Char) :-
    char_code(Char, Code).

%% to_json_chars(+Chars, -JSONChars)
%
% Pretty-print a raw JSON char list `Chars` through `jq` and
% return the formatted output as `JSONChars`. Writes through
% two temporary files, the second of which is removed before
% returning.
to_json_chars(Chars, JSONChars) :-
    chars_si(Chars),
    atom_chars(Atom, Chars),
    temporary_file("beautify_json", ".json", JsonFile),
    open(JsonFile, write, WriteToJsonStream, [type(text)]),
    write_term(WriteToJsonStream, Atom, [quoted(false)]),

    once(beautify_json(JsonFile, JSONChars)),
    log_debug([JSONChars]).

%% beautify_json(+JsonFile, -JSONChars)
%
% Run `cat | sed | jq` against `JsonFile`, read the formatted
% output back as `JSONChars`, and remove the source temp file.
beautify_json(TempFile, JSONChars) :-
    temporary_file("beautify_json", FormattedJsonFile),
    char_code(AntiSlash, 92),
    char_code(DoubleQuote, 34),
    append([
        [AntiSlash], "cat '", TempFile, "' | ",
        " sed 's#", [AntiSlash], [AntiSlash], [DoubleQuote],
        "#", [DoubleQuote], "#g' | ",
        [AntiSlash],"jq > ", FormattedJsonFile
    ], Cmd),

    char_code(Eol, 10),
    log_debug(['Command: ', Eol, Cmd]),

    shell(Cmd, CmdExecutionStatus),
    if_(
        dif(CmdExecutionStatus, 0),
        throw(unexpected_command_exit_code('Failed to beautify JSON')),
        remove_temporary_file(TempFile)
    ),

    once(open(FormattedJsonFile, read, ReadFromJsonStream, [type(text)])),
    read_stream(ReadFromJsonStream, JSONChars),
    remove_temporary_file(FormattedJsonFile).

%% by_key(+Key, +Pairs, -Value)
%
% Look up `Key` in a JSON-DCG pair list and unify `Value` with
% the inner value (the unwrapped char list, not the `string/1`
% wrapper). Throws `key_not_found/1` when the key is absent.
by_key(Key, [], _) :-
    throw(key_not_found(Key)).
by_key(Key, [string(OtherKey)-_Value|Pairs], Value) :-
    dif_si(Key, OtherKey),
    by_key(Key, Pairs, Value).
by_key(Key, [string(Key)-string(Value)|_Pairs], Value).
by_key(Key, [string(Key)-pairs(Value)|_Pairs], Value).

%% keys(+Pairs, +KeysIn, -KeysOut)
%
% Difference-list accumulator that gathers every top-level key
% name from a JSON-DCG pair list as a list of char lists.
keys([], Keys, Keys).
keys([string(KeyChars)-_|RemainingKeys], KeysIn, KeysOut) :-
    append([KeysIn, [KeyChars]], Ks),
    keys(RemainingKeys, [], Keys),
    append([Ks, Keys], KeysOut).

%% wrapped_pairs_to_assoc(+Pairs, -Assoc)
%
% Unwrap a `pairs(_)`-wrapped JSON object then convert it to an
% assoc keyed by atom keys.
wrapped_pairs_to_assoc(pairs(UnwrappedPairs), Assoc) :-
    once(pairs_to_assoc(UnwrappedPairs, Assoc)).

%% pairs_to_assoc(+Pairs, -Assoc)
%
% Convert a JSON-DCG object pair list to a prolog assoc keyed
% by atom keys. Nested `pairs(_)` values become nested assocs.
pairs_to_assoc(Pairs, Assoc) :-
    once(maplist(without_type, Pairs, PairsOut)),
    list_to_assoc(PairsOut, Assoc).

% without_type(+Pair, -PairWithoutType).
%
% Relates left string in chars representation with an atom
% Relates right boolean, number, string, list with the same type
% Relates right pairs with an assoc
without_type(string(Key)-boolean(Value), KeyAtom-Value) :-
    log_debug([kv:string(Key)-boolean(Value)]),
    atom_chars(KeyAtom, Key).
without_type(string(Key)-string(Value), KeyAtom-Value) :-
    log_debug([kv:string(Key)-string(Value)]),
    atom_chars(KeyAtom, Key).
without_type(string(Key)-number(Value), KeyAtom-Value) :-
    log_debug([kv:string(Key)-number(Value)]),
    atom_chars(KeyAtom, Key).
without_type(string(Key)-list(Value), KeyAtom-Value) :-
    log_debug([kv:string(Key)-list(Value)]),
    atom_chars(KeyAtom, Key).
without_type(string(Key)-pairs(Value), KeyAtom-ValueAssoc) :-
    atom_chars(KeyAtom, Key),
    pairs_to_assoc(Value, ValueAssoc).
without_type(Left-Right, UnknownKind) :-
    throw(cannot_remove_type(Left, Right)-because_of(UnknownKind)).

%% unwrap_pairs(+Pairs, -UnwrappedPairs)
%
% Strip the outer `pairs(_)` wrapper used by the JSON DCG.
unwrap_pairs(pairs(UnwrappedPairs), UnwrappedPairs).
