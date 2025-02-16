:- module(repository_dcgs, [
    rows//1,
    to_json/3
]).

:- use_module(library(lists)).
:- use_module(library(si)).

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

%% to_json(+Headers, +Row, -Json).
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
key_typed_value(Key, Type, Value, TypedValue) :-
    (   Type = "string"
    ->  TypedValue = string(Key)-string(Value)
    ;   Type = "number"
    ->  TypedValue = string(Key)-number(Value)
    ;   Type = "boolean"
    ->  TypedValue = string(Key)-boolean(Value)
    ;   Type = "list"
    ->  TypedValue = string(Key)-list(Value)
    ;   Type = "pairs"
    ->  TypedValue = string(Key)-pairs(Value)
    ;   throw(unsupported_type) ).

%% value_type(+Header, -Type, -Name).
value_type(Header, Type, Name) :-
    append([Type, "__", Name], Header).
