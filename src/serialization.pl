:- module(serialization, [by_key/3]).
:- use_module(library(si)).

% Extract the value of a key from a list of key-value pairs.
%
% by_key(+Key, +Pairs, -Value).
by_key(Key, [], _) :-
    throw(key_not_found(Key)).
by_key(Key, [string(OtherKey)-_Value|Pairs], Value) :-
    dif_si(Key, OtherKey),
    by_key(Key, Pairs, Value).
by_key(Key, [string(Key)-string(Value)|_Pairs], Value).