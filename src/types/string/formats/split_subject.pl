:- module(split_subject, [split_subject/3]).

/**
Split a char list into labels around a separator character.

Walks the input one character at a time, accumulating each
label as a char list and emitting a labels list whenever the
separator is hit.
*/

:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(has_only_ascii_chars, [
    has_only_ascii_chars/1,
    must_be_ascii_char/1
]).

:- use_module(must_be_ground, [must_be_ground/1]).
:- use_module(must_be_chars, [must_be_chars/1]).

% subject_and_separator_must_be_valid(+Subject, +Separator).
%
% subject_and_separator_must_be_valid(+Subject, +Separator).
subject_and_separator_must_be_valid(Subject, Separator) :-
    must_be_chars(Subject),
    must_be_ground(Separator),
    length(Subject, Length),
    once(first_char_for_length(Length, Subject, _Char)),
    has_only_ascii_chars(Subject),
    must_be_ascii_char(Separator).

first_char_for_length(Length, [Char|_Rest], Char) :- Length > 1.
first_char_for_length(Length, [Char], Char) :- Length =< 1.

% The overall handle is split in to multiple segments
% (referred to as "labels" in standards documents),
% separated by ASCII periods (.)
%
% split_subject(+Subject, +Separator, +_LastLabel, -AccIn, +AccIn).
split_subject("", _Separator, _LastLabel, AccIn, AccIn).
split_subject(Subject, Separator, LabelAcc, AccIn, AccOut) :-
    subject_and_separator_must_be_valid(Subject, Separator),
    length(Subject, 1),
    Subject = [Char],
    if_(
        Char = Separator,
        (   NextLabelAcc = "",
            append([AccIn, [LabelAcc], [""]], NextAccIn) ),
        (   append([LabelAcc, [Char]], NextLabelAcc),
            append([AccIn, [NextLabelAcc]], NextAccIn) )
    ),
    split_subject("", Separator, NextLabelAcc, NextAccIn, AccOut).
split_subject(Subject, Separator, LabelAcc, AccIn, AccOut) :-
    subject_and_separator_must_be_valid(Subject, Separator),
    Subject = [Char|Rest],
    if_(
        Char = Separator,
        (   NextLabelAcc = "",
            append(AccIn, [LabelAcc], NextAccIn) ),
        (   append([LabelAcc, [Char]], NextLabelAcc),
            AccIn = NextAccIn )
    ),
    split_subject(Rest, Separator, NextLabelAcc, NextAccIn, AccOut).

%% split_subject(+Subject, +Separator, -Labels)
%
% Unify `Labels` with the list of char lists obtained by
% splitting `Subject` on `Separator`.
split_subject(Subject, Separator, Labels) :-
    once(split_subject(Subject, Separator, "", [], Labels)).
