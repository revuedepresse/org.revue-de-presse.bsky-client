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

/**
Handle validator for the AT Protocol.

Implements every rule from the [Handle
Identifier Syntax](https://atproto.com/specs/handle): length
limits (≤ 253 chars overall, 1–63 per label), at least two
dot-separated labels, ASCII letters/digits/hyphens only, no
leading nor trailing dots, no leading nor trailing hyphens,
no leading digit on the final label, and a configurable
disallowed-TLD list whose `.test` entry is only enforced in
production.
*/

:- use_module(library(debug)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
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

%% split_subject(+Subject, +Separator, -Labels)
%
% Re-exported from [[split_subject]]. Splits `Subject` on
% `Separator` into a list of labels.

%% is_valid_handle(+Subject)
%
% Succeed iff `Subject` is a valid AT Protocol handle. See
% [String Formats](https://atproto.com/specs/lexicon#string-formats)
% and [Handle Identifier
% Syntax](https://atproto.com/specs/handle). The check is the
% conjunction of every other exported predicate in this
% module.
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

%% has_only_allowed_chars(+Subject)
%
% Succeed iff every character of `Subject` is an ASCII letter
% (case-insensitive), digit, or hyphen. Handles are not
% case-sensitive and should be normalized to lowercase.
has_only_allowed_chars(Subject) :-
    maplist(must_be_allowed_char, Subject).
has_only_allowed_chars(Subject) :-
    \+ maplist(must_be_allowed_char, Subject),
    throw(type_error('Subject must contain only allowed characters.')).

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



%% has_valid_length(+Subject)
%
% Succeed iff `Subject` is at most 253 characters long.
% "Bare" top-level domains are not allowed as handles, even
% if valid hostnames or DNS names.
has_valid_length(Subject) :-
    length(Subject, Length),
    Length #=< 253.

%% with_labels_having_valid_length(+Subject)
%
% Succeed iff every dot-separated label of `Subject` is
% between 1 and 63 characters long, exclusive of the periods.
with_labels_having_valid_length(Subject) :-
    split_subject(Subject, '.', Labels),
    maplist(label_has_valid_length, Labels).

% Each segment must have at least 1 and at most 63 characters
% (not including the periods).
%
% label_has_valid_length(+Subject).
label_has_valid_length(Subject) :-
    length(Subject, Length),
    Length >= 1,
    Length =< 63.
label_has_valid_length(Subject) :-
    length(Subject, Length),
    \+ ( Length >= 1, Length =< 63 ),
    throw(invalid_label_length).

% The last segment (the "top level domain") can not start with a numeric digit
%
% with_last_label_not_starting_with_digit(+Subject).
with_last_label_not_starting_with_digit(Subject) :-
    split_subject(Subject, '.', Labels),
    reverse(Labels, [LastLabel|_]),
    not_starting_with_digit(LastLabel).

%% with_last_label_distinct_from_disallowed_top_level_domain(+Subject)
%
% Succeed iff the last label of `Subject` is not in the
% disallowed-TLD list (`alt`, `arpa`, `example`, `internal`,
% `local`, `localhost`, `onion`, plus `test` outside the
% `development` / `testing` environments).
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

%% has_two_labels_at_least(+Subject)
%
% Succeed iff `Subject` has at least two dot-separated labels.
has_two_labels_at_least(Subject) :-
    split_subject(Subject, '.', Labels),
    length(Labels, N),
    assert_two_labels(N).

assert_two_labels(N) :- N #>= 2.
assert_two_labels(N) :- N #< 2, throw(error_must_have_two_labels_at_least).

%% has_no_proceeding_ascii_period(+Subject)
%
% Succeed iff `Subject` does not start with an ASCII period.
has_no_proceeding_ascii_period(Subject) :-
    must_not_start_with(Subject, '.').

%% has_no_trailing_ascii_period(+Subject)
%
% Succeed iff `Subject` does not end with an ASCII period.
% "Trailing dot" syntax for DNS names is not allowed for
% handles.
has_no_trailing_ascii_period(Subject) :-
    must_not_end_with(Subject, '.').

%% not_starting_with_hyphen(+Subject)
%
% Succeed iff `Subject` does not start with a hyphen.
not_starting_with_hyphen(Subject) :-
    must_not_start_with(Subject, '-').

%% not_ending_with_hyphen(+Subject)
%
% Succeed iff `Subject` does not end with a hyphen.
not_ending_with_hyphen(Subject) :-
    must_not_end_with(Subject, '-').

%% not_starting_with_digit(+Subject)
%
% Succeed iff the first character of `Subject` is not an
% ASCII digit. Throws `error_must_not_start_with_digit` on
% digit-led inputs.
not_starting_with_digit([FirstChar|_]) :-
    char_code(FirstChar, FirstCharCode),
    char_code('0', DigitStartCode),
    char_code('9', DigitEndCode),
    assert_not_digit(FirstCharCode, DigitStartCode, DigitEndCode).

assert_not_digit(Code, Start, _End) :- Code < Start.
assert_not_digit(Code, _Start, End) :- Code > End.
assert_not_digit(Code, Start, End) :-
    Code >= Start, Code =< End,
    throw(error_must_not_start_with_digit).

%% not_disallowed_top_level_domain(+TopLevelDomain)
%
% Succeed iff `TopLevelDomain` is not in the disallowed-TLD
% list. The list grows with `"test"` outside the
% `"development"` and `"testing"` environments (so production
% rejects `.test`, but local dev allows it).
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
    select_disallowed_top_level_domains(Environment, BaseDisallowedTopLevelDomains, DisallowedTopLevelDomains),
    assert_top_level_domain_allowed(TopLevelDomain, DisallowedTopLevelDomains).

select_disallowed_top_level_domains("development", Base, Base).
select_disallowed_top_level_domains("testing", Base, Base).
select_disallowed_top_level_domains(Env, Base, Out) :-
    dif(Env, "development"),
    dif(Env, "testing"),
    append([Base, ["test"]], Out).

assert_top_level_domain_allowed(TopLevelDomain, Disallowed) :-
    \+ member(TopLevelDomain, Disallowed).
assert_top_level_domain_allowed(TopLevelDomain, Disallowed) :-
    member(TopLevelDomain, Disallowed),
    throw(disallowed_top_level_domain(TopLevelDomain)).