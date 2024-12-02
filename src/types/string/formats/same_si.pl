:- module(same_si, [same_si/2]).

:- use_module(library(si)).

same_si(Left, Right) :-
    \+ dif_si(Left, Right)
    ->  true
    ;   throw(error_must_be_same(Left,Right)).
