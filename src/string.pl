:- module(string, [concat_as_string/3]).

/**
Heterogeneous string concatenation.

Joins a list of mixed atoms and char lists into a single char
list by converting atoms with `atom_chars/2` and appending
char lists verbatim. Used everywhere a request URL, log
message, or shell command is assembled from a mix of literal
strings and configuration atoms.
*/

:- use_module(library(lists)).
:- use_module(library(si)).

%% concat_as_string(+List, +ConcatenationIn, -ConcatenationOut)
%
% Concatenate a heterogeneous `List` of atoms and char lists
% onto `ConcatenationIn`, returning a single char list as
% `ConcatenationOut`. Atoms are decomposed with `atom_chars/2`;
% char lists are appended verbatim.
concat_as_string([], In, In).
concat_as_string([Head|Tail], ConcatenationIn, ConcatenationOut) :-
    head_chars(Head, HeadChars),
    append([ConcatenationIn, HeadChars], Out),
    concat_as_string(Tail, Out, ConcatenationOut).

head_chars(Head, HeadChars) :- atomic_si(Head), atom_chars(Head, HeadChars).
head_chars(Head, Head) :- \+ atomic_si(Head).