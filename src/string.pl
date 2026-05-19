:- module(string, [concat_as_string/3]).

:- use_module(library(lists)).
:- use_module(library(si)).

% Concatenate a list of strings or atoms into a single string,
% by converting atoms to chars
%
% concat_as_string(+List, +ConcatenationIn, -ConcatenationOut).
concat_as_string([], In, In).
concat_as_string([Head|Tail], ConcatenationIn, ConcatenationOut) :-
    head_chars(Head, HeadChars),
    append([ConcatenationIn, HeadChars], Out),
    concat_as_string(Tail, Out, ConcatenationOut).

head_chars(Head, HeadChars) :- atomic_si(Head), atom_chars(Head, HeadChars).
head_chars(Head, Head) :- \+ atomic_si(Head).