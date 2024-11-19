:- module(must_be_lowercase_alpha, [must_be_lowercase_alpha/1]).

:- use_module(library(clpz)).
:- use_module(must_be_ground, [must_be_ground/1]).

must_be_lowercase_alpha(Char) :-
    must_be_ground(Char),

    char_code('a', LowerAlphaStartCode),
    char_code('z', LowerAlphaEndCode),
    char_code(Char, Code),

    Code #>= LowerAlphaStartCode,
    Code #=< LowerAlphaEndCode.