:- module(repository_dcgs, [
    rows//1,
    to_json/3
]).

/**
DCG and helpers for parsing pipe-delimited query results.

The wire client writes each row of a multi-row SELECT to a
tempfile as `field|field|...\n`. The `rows//1` DCG here
reverses that encoding back into a list of field lists, and
`to_json/3` converts each row into a typed pair list keyed by
column-header names of the shape `<type>__<name>` (e.g.
`number__id`, `string__handle`).
*/

:- use_module(library(lists)).
:- use_module(library(si)).

%% rows(-Rows)//
%
% DCG: consume a stream of `field|field|...\n` lines and
% yield `Rows` as a list of field lists.
rows([R|Rows]) --> fields(R), eol, rows(Rows).
rows([Fields]) --> fields(Fields).

fields([F|Fs]) --> field(F), sep(_Seps), fields(Fs).
fields([F]) -->
    field(F), {
        char_code(Eol0, 10),
        dif_si(F, Eol0),
        char_code(Eol1, 13),
        dif_si(F, Eol1)
    }.

field([R|Rows]) --> single_character(R), field(Rows).
field([R]) --> single_character(R), sep(_).
field([R]) --> single_character(R), eol.
field([R]) --> single_character(R).

single_character(R) -->
    [R], {
        length([R], 1),
        dif_si(R, '|'),
        char_code(Eol0, 10),
        dif_si(R, Eol0),
        char_code(Eol1, 13),
        dif_si(R, Eol1)
    }.
single_character([" "]) --> " ".

sep([_Sep|Seps]) --> ['|'], sep(Seps).
sep([_Sep]) --> ['|'].

eol --> [Char], { char_code(Char, 10) }.
eol --> [Char], { char_code(Char, 13) }.

%% to_json(+Headers, +Row, -Json)
%
% Pair each column of `Row` with its header by index and emit
% `Json` as a list of `string(Name)-Type(Value)` pairs, where
% `Type` is one of `string`, `number`, `boolean`, `list`,
% `pairs`. Headers are expected in `Type__Name` form.
to_json(Headers, Row, Json) :-
    foldl(build_pair(Row), Headers, 0-[], _-Json).

%% build_pair(+Row, +Header, +In-PairsIn, -Out-PairsOut).
build_pair(Row, Header, In-PairsIn, Out-PairsOut) :-
    nth0(In, Row, ColumnValue),
    value_type(Header, Type, Name),
    key_typed_value(Name, Type, ColumnValue, KeyValue),
    append([PairsIn, [KeyValue]], PairsOut),
    Out #= In + 1.

%% key_typed_value(+Key, +Type, +Value, -TypedValue).
key_typed_value(Key, "string",  Value, string(Key)-string(Value)).
key_typed_value(Key, "number",  Value, string(Key)-number(Value)).
key_typed_value(Key, "boolean", Value, string(Key)-boolean(Value)).
key_typed_value(Key, "list",    Value, string(Key)-list(Value)).
key_typed_value(Key, "pairs",   Value, string(Key)-pairs(Value)).
key_typed_value(_, Type, _, _) :-
    \+ member(Type, ["string", "number", "boolean", "list", "pairs"]),
    throw(unsupported_type).

%% value_type(+Header, -Type, -Name).
value_type(Header, Type, Name) :-
    append([Type, "__", Name], Header).
