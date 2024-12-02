:- module(handle, [
    has_no_proceeding_ascii_period/1,
    has_no_trailing_ascii_period/1,
    has_only_allowed_chars/1,
    has_two_labels_at_least/1,
    has_valid_length/1,
    is_valid_handle/1,
    not_disallowed_top_level_domain/1,
    not_ending_with_hyphen/1,
    not_starting_with_hyphen/1,
    not_starting_with_digit/1,
    split_subject/3,
    with_labels_having_valid_length/1,
    with_last_label_distinct_from_disallowed_top_level_domain/1
]).

:- use_module(library(debug)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(si)).

:- use_module('../../../logger', [
    log_info/1
]).
:- use_module('../../../configuration', [
    environment/1
]).
:- use_module(must_be_ground, [must_be_ground/1]).
:- use_module(must_be_lowercase_alpha, [must_be_lowercase_alpha/1]).
:- use_module(must_not_end_with, [must_not_end_with/2]).
:- use_module(must_not_start_with, [must_not_start_with/2]).
:- use_module(split_subject, [split_subject/3]).

% See [String Formats](https://atproto.com/specs/lexicon#string-formats)
% See [Handle Identifier Syntax](https://atproto.com/specs/handle)
%
% is_valid_handle(+Subject).
is_valid_handle(Subject) :-
    has_valid_length(Subject),
    has_two_labels_at_least(Subject),
    has_no_trailing_ascii_period(Subject),
    has_no_proceeding_ascii_period(Subject),
    with_labels_having_valid_length(Subject),
    with_labels_having_allowed_chars(Subject),
    with_labels_not_starting_nor_ending_with_hyphen(Subject),
    with_last_label_not_starting_with_digit(Subject),
    with_last_label_distinct_from_disallowed_top_level_domain(Subject).

% The allowed characters are ASCII letters (a-z), digits (0-9), and hyphens (-).
% Handles are not case-sensitive, and should be normalized to lowercase
% (that is, normalize ASCII A-Z to a-z)
%
% with_labels_having_allowed_chars(+Subject).
with_labels_having_allowed_chars(Subject) :-
    split_subject(Subject, '.', Labels),
    maplist(has_only_allowed_chars, Labels).

% has_only_allowed_chars(+Subject).
has_only_allowed_chars(Subject) :-
    \+  maplist(must_be_allowed_char, Subject)
    ->  throw(type_error('Subject must contain only allowed characters.'))
    ;   true.

% must_be_allowed_char(+Char).
must_be_allowed_char(Char) :-
    char_code('A', AlphaStartCode),
    char_code('Z', AlphaEndCode),

    char_code('0', DigitStartCode),
    char_code('9', DigitEndCode),

    char_code('-', HyphenCode),

    char_code(Char, Code),

    (   must_be_lowercase_alpha(Char)
    ;   Code #>= AlphaStartCode, Code #=< AlphaEndCode
    ;   Code #>= DigitStartCode, Code #=< DigitEndCode
    ;   Code #= HyphenCode ).



% […], and can be at most 253 characters long
% (in practice, handles may be restricted to a slightly shorter length)
% That is, "bare" top-level domains are not allowed as handles,
% even if valid "hostnames" and "DNS names."
%
% has_valid_length(+Subject).
has_valid_length(Subject) :-
    length(Subject, Length),
    Length #=< 253.

% with_labels_having_valid_length(+Subject).
with_labels_having_valid_length(Subject) :-
    split_subject(Subject, '.', Labels),
    maplist(label_has_valid_length, Labels).

% Each segment must have at least 1 and at most 63 characters
% (not including the periods).
%
% label_has_valid_length(+Subject).
label_has_valid_length(Subject) :-
    length(Subject, Length),
    (
        Length #>= 1,
        Length #=< 63
    )
    ->  true
    ;   throw(invalid_label_length).

% The last segment (the "top level domain") can not start with a numeric digit
%
% with_last_label_not_starting_with_digit(+Subject).
with_last_label_not_starting_with_digit(Subject) :-
    split_subject(Subject, '.', Labels),
    reverse(Labels, [LastLabel|_]),
    not_starting_with_digit(LastLabel).

% The .test TLD is intended for examples, testing, and development.
% It may be used in atproto development, but should fail in real-world environments.
%
% with_last_label_distinct_from_disallowed_top_level_domain(+Subject).
with_last_label_distinct_from_disallowed_top_level_domain(Subject) :-
    split_subject(Subject, '.', Labels),
    reverse(Labels, [LastLabel|_]),
    not_disallowed_top_level_domain(LastLabel).

% Segments can not start or end with a hyphen
%
% with_labels_not_starting_nor_ending_with_hyphen(+Subject).
with_labels_not_starting_nor_ending_with_hyphen(Subject) :-
    split_subject(Subject, '.', Labels),
    maplist(must_not_start_nor_end_with_hyphen, Labels).

% must_not_start_nor_end_with_hyphen(+Subject).
must_not_start_nor_end_with_hyphen(Subject) :-
    not_starting_with_hyphen(Subject),
    not_ending_with_hyphen(Subject).

% […], and there must be at least two segments.
%
% has_two_labels_at_least(+Subject).
has_two_labels_at_least(Subject) :-
    split_subject(Subject, '.', Labels),
    length(Labels, N),
    (   N #< 2
    ->  throw(error_must_have_two_labels_at_least)
    ;   true ).

% No proceeding or trailing ASCII periods are allowed
%
% has_no_proceeding_ascii_period(+Subject).
has_no_proceeding_ascii_period(Subject) :-
    must_not_start_with(Subject, '.').

% "Trailing dot" syntax for DNS names is not allowed for handles.
%
% has_no_trailing_ascii_period(+Subject).
has_no_trailing_ascii_period(Subject) :-
    must_not_end_with(Subject, '.').

% Segments can not start […] with a hyphen
%
% not_starting_with_hyphen(+Subject).
not_starting_with_hyphen(Subject) :-
    must_not_start_with(Subject, '-').

% […] or end with a hyphen
%
% not_ending_with_hyphen(+Subject).
not_ending_with_hyphen(Subject) :-
    must_not_end_with(Subject, '-').

% not_starting_with_digit(+Subject).
not_starting_with_digit([FirstChar|_]) :-
    char_code(FirstChar, FirstCharCode),
    char_code('0', DigitStartCode),
    char_code('9', DigitEndCode),

    ( (
        FirstCharCode #>= DigitStartCode,
        FirstCharCode #=< DigitEndCode
    )
    ->  throw(error_must_not_start_with_digit)
    ;   true ).

% not_disallowed_top_level_domain(+TopLevelDomain).
not_disallowed_top_level_domain(TopLevelDomain) :-
    must_be_ground(TopLevelDomain),

    BaseDisallowedTopLevelDomains = [
        "alt",
        "arpa",
        "example",
        "internal",
        "local",
        "localhost",
        "onion"
    ],

    environment(Environment),

    ( (   Environment = "development"
        ;   Environment = "testing" )
    ->  DisallowedTopLevelDomains = BaseDisallowedTopLevelDomains
    ;   append([BaseDisallowedTopLevelDomains, ["test"]], DisallowedTopLevelDomains) ),

    (   member(TopLevelDomain, DisallowedTopLevelDomains)
    ->  throw(disallowed_top_level_domain(TopLevelDomain))
    ;   true ).