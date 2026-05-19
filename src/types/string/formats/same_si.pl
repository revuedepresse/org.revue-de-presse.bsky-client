:- module(same_si, [same_si/2]).

/**
Equality with sound iso-friendly semantics.

`same_si/2` uses `dif_si/2` to assert that two terms are
indistinguishable up to instantiation, throwing
`error_must_be_same/2` on mismatch. The shape lets call sites
treat equality the same way as the other `must_*` validators.
*/

:- use_module(library(si)).

%% same_si(+Left, +Right)
%
% Succeed iff `Left` and `Right` are not distinguishable by
% `dif_si/2`; throw `error_must_be_same/2` otherwise.
same_si(Left, Right) :-
    \+ dif_si(Left, Right).
same_si(Left, Right) :-
    dif_si(Left, Right),
    throw(error_must_be_same(Left, Right)).
