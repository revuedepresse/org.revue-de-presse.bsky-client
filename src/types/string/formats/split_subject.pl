:- module(split_subject, [split_subject/3]).

:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(has_only_ascii_chars, [
    has_only_ascii_chars/1,
    must_be_ascii_char/1
]).

:- use_module('../../../memoize', [
    memoize_goal/2,
    memoized_goal/2
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
    (   Length #> 1
    ->  Subject = [Char|_Rest]
    ;   Subject = [Char] ),

    has_only_ascii_chars(Subject),
    must_be_ascii_char(Separator).

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

% split_subject(+Subject, +Separator, -Labels).
split_subject(Subject, Separator, Labels) :-
    memoized_goal(split_subject:split_subject(Subject, Separator, "", [], Labels), [Subject, Separator, "", [], Labels])
    ->  true
    ;   memoize_goal(split_subject:split_subject(Subject, Separator, "", [], Labels), [Subject, Separator, "", [], Labels]).
