:- module(must_be_lowercase_alpha, [must_be_lowercase_alpha/1]).

/**
Type guard: a single character must be in `a..z`.

Used by handle and DID validation to constrain individual
characters in labels and method names.
*/

:- use_module(library(clpz)).
:- use_module(must_be_ground, [must_be_ground/1]).

%% must_be_lowercase_alpha(+Char)
%
% Succeed iff `Char` is a single ground character in the
% inclusive range `'a' .. 'z'`. Fails (no throw) on chars
% outside the range so callers can use it as a clpz-style
% guard inside a disjunction.
must_be_lowercase_alpha(Char) :-
    must_be_ground(Char),

    char_code('a', LowerAlphaStartCode),
    char_code('z', LowerAlphaEndCode),
    char_code(Char, Code),

    Code #>= LowerAlphaStartCode,
    Code #=< LowerAlphaEndCode.