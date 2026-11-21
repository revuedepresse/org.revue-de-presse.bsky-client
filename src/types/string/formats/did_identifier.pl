:- module(did_identifier, [
    has_only_allowed_chars/1,
    is_valid_did_identifier/1,
    must_have_valid_identifier_format/1,
    must_have_valid_method_format/1,
    must_have_valid_percent_encoding/1
]).

/**
DID identifier validator.

Implements [AT Protocol DID Identifier
Syntax](https://atproto.com/specs/did#at-protocol-did-identifier-syntax):
ASCII-only character set, mandatory `did:` prefix, three
colon-separated segments (`did:<method>:<identifier>`), allowed
characters, allowed percent-encoded codes, and a small allow
list of supported methods (`web`, `plc`).

Written in the reified style src/link_shortener.pl sets out:
every rule is a predicate whose last argument is a truth
value, and `if_/3` chooses between succeeding and throwing the
error that names what was wrong. No negation as failure, so a
rule can be asked rather than only tried, and no `once/1`:
each rule has one answer, so there is no second one to cut
away.
*/

:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module('../../../logger', [log_info/1]).
:- use_module(has_only_ascii_chars, [has_only_ascii_chars/1]).
:- use_module(must_be_chars, [must_be_chars/1]).
:- use_module(must_be_ground, [must_be_ground/1]).
:- use_module(must_not_end_with, [must_not_end_with/2]).
:- use_module(must_start_with, [must_start_with/2]).
:- use_module(split_subject, [split_subject/3]).

%% is_valid_did_identifier(+Subject)
%
% Succeed iff `Subject` satisfies every step of the [AT
% Protocol DID Identifier
% Syntax](https://atproto.com/specs/did#at-protocol-did-identifier-syntax):
% ASCII-only, allowed character set, `"did:"` prefix, valid
% method format, valid identifier format, and a method we
% support.
is_valid_did_identifier(Subject) :-
    has_only_ascii_chars(Subject),
    has_only_allowed_chars(Subject),
    must_start_with(Subject, "did:"),
    must_have_valid_method_format(Subject),
    must_have_valid_identifier_format(Subject),
    must_be_supported_method(Subject).

%% has_only_allowed_chars(+Subject)
%
% Succeed iff every character of `Subject` is in the DID
% allowed alphabet — ASCII alphanumerics plus `:`, `.`, `-`,
% `%`, `_`. Throws `type_error/1` on the first disallowed
% character.
has_only_allowed_chars(Subject) :-
    if_(all_allowed_chars_t(Subject),
        true,
        throw(type_error('Subject must contain only allowed characters.'))).

all_allowed_chars_t([], true).
all_allowed_chars_t([Char|Chars], Truth) :-
    if_(allowed_char_t(Char), all_allowed_chars_t(Chars, Truth), Truth = false).

% Characters are read as their codes rather than compared
% against literals: three of the punctuation marks the
% alphabet allows (`:`, `.`, `-`) are operator atoms Scryer
% will not read in argument position even quoted.
%
% allowed_char_t(+Char, -Truth).
allowed_char_t(Char, Truth) :-
    char_code(Char, Code),
    if_(code_in_t(0x61, 0x7A, Code),            % a-z
        Truth = true,
        if_(code_in_t(0x41, 0x5A, Code),        % A-Z
            Truth = true,
            if_(code_in_t(0x30, 0x39, Code),    % 0-9
                Truth = true,
                % : . - % _
                memberd_t(Code, [0x3A, 0x2E, 0x2D, 0x25, 0x5F], Truth)))).

%% must_have_valid_identifier_format(+Subject)
%
% Verify the third segment of a `did:<method>:<id>` identifier:
% allowed character set, no trailing `%`, and well-formed
% percent encoding.
must_have_valid_identifier_format(Subject) :-
    split_subject(Subject, ':', Segments),
    length(Segments, SegmentsLength),
    assert_segments_at_least_3(SegmentsLength),
    assert_segments_at_most_3(SegmentsLength),
    nth0(2, Segments, Identifier),
    % The identifier's alphabet is the subject's alphabet, and
    % has_only_allowed_chars/1 already throws on a character
    % outside it, so there is nothing left to assert here.
    has_only_allowed_chars(Identifier),
    catch(
        must_not_end_with(Subject, '%'),
        _E,
        throw(error_identifier_must_not_end_with('%'))
    ),
    must_have_valid_percent_encoding(Identifier).

assert_segments_at_least_3(N) :-
    if_(at_least_t(3, N), true, throw(error_not_enough_segments)).

assert_segments_at_most_3(N) :-
    if_(at_most_t(3, N), true, throw(error_too_many_segments)).

must_have_at_least_n_characters(N, Subject) :-
    length(Subject, NChars),
    if_(at_least_t(N, NChars),
        true,
        throw(error_must_have_at_least_n_characters(N))).

%% must_have_valid_percent_encoding(+Subject)
%
% Split `Subject` on `%`, then verify that each segment that
% follows a `%` is at least two characters long and that the
% leading two characters form an allowed hex code.
%
% The lengths are all checked before any code is, so a subject
% carrying both faults is reported as the truncated escape it
% is rather than as an unknown code.
must_have_valid_percent_encoding(Subject) :-
    split_subject(Subject, '%', Segments),
    Subject = [FirstChar|_Rest],
    encoded_segments(FirstChar, Segments, Encoded),
    maplist(must_have_at_least_n_characters(2), Encoded),
    maplist(must_be_valid_percent_encoding, Encoded).

% Splitting on `%` yields one segment more than there are
% escapes: the text that precedes the first `%`. That text
% carries no code to check — unless the subject opens with the
% `%` itself, in which case the first segment is the empty
% string before it and is read as an escape like the rest.
encoded_segments(FirstChar, Segments, Encoded) :-
    if_(percent_t(FirstChar),
        Encoded = Segments,
        ( Segments = [_Leading|Rest], Encoded = Rest )).

percent_t(Char, Truth) :-
    char_code(Char, Code),
    =(0x25, Code, Truth).

% must_be_valid_percent_encoding(+Segment).
must_be_valid_percent_encoding(Segment) :-
    length(Segment, N),
    assert_percent_pair_length(N),
    Segment = [FirstChar,SecondChar|_Rest],
    assert_percent_code_allowed([FirstChar,SecondChar]).

assert_percent_pair_length(N) :-
    if_(at_least_t(2, N),
        true,
        throw(error_percent_encoding_must_have_two_hex_chars)).

% The codes a DID identifier may escape, in both the upper and
% the lower case the spec allows for the hex digits.
allowed_percent_codes([
    "3A", "2F", "3F", "23", "5B", "5D", "2A", "2B", "2C", "3B", "3D",
    "3a", "2f", "3f", "5b", "5d", "2a", "2b", "2c", "3b", "3d",
    "21", "24", "25", "26", "27", "28", "29", "20", "40"
]).

assert_percent_code_allowed(Code) :-
    allowed_percent_codes(Codes),
    if_(memberd_t(Code, Codes), true, throw(error_invalid_percent_encoding)).

must_not_be_empty(Subject) :-
    length(Subject, N),
    if_(N = 0, throw(error_must_not_be_empty), true).

%% must_have_valid_method_format(+Subject)
%
% Verify the second segment of a `did:<method>:<id>` identifier
% is a non-empty lowercase-alpha-only string.
must_have_valid_method_format(Subject) :-
    split_subject(Subject, ':', Segments),
    length(Segments, SegmentsLength),
    maplist(must_not_be_empty, Segments),
    assert_segments_at_least_3(SegmentsLength),
    nth0(1, Segments, Method),
    assert_method_lowercase_alpha(Method).

% A method written in any upper case is a different name, not
% a variant of the same one, so the case is a format error
% rather than something to normalise.
assert_method_lowercase_alpha(Method) :-
    must_be_ground(Method),
    if_(all_lowercase_alpha_t(Method),
        true,
        throw(error_method_must_be_lowercase_alpha_only)).

all_lowercase_alpha_t([], true).
all_lowercase_alpha_t([Char|Chars], Truth) :-
    if_(lowercase_alpha_t(Char),
        all_lowercase_alpha_t(Chars, Truth),
        Truth = false).

lowercase_alpha_t(Char, Truth) :-
    char_code(Char, Code),
    code_in_t(0x61, 0x7A, Code, Truth).         % a-z

% must_be_supported_method(+Subject).
must_be_supported_method(Subject) :-
    split_subject(Subject, ':', Segments),
    length(Segments, SegmentsLength),
    assert_segments_at_least_3(SegmentsLength),
    nth0(1, Segments, Method),
    assert_method_supported(Method).

supported_methods(["web", "plc"]).

assert_method_supported(Method) :-
    supported_methods(Methods),
    if_(memberd_t(Method, Methods),
        true,
        throw(error_method_must_be_supported)).

%% code_in_t(+Low, +High, +Value, -Truth)
%
% `true` when `Low =< Value =< High`.
code_in_t(Low, High, Value, Truth) :-
    zcompare(BelowOrder, Low, Value),
    zcompare(AboveOrder, Value, High),
    range_truth(BelowOrder, AboveOrder, Truth).

range_truth(<, <, true).
range_truth(<, =, true).
range_truth(<, >, false).
range_truth(=, <, true).
range_truth(=, =, true).
range_truth(=, >, false).
range_truth(>, <, false).
range_truth(>, =, false).
range_truth(>, >, false).

%% at_least_t(+Bound, +Value, -Truth) and at_most_t(+Bound, +Value, -Truth)
%
% `true` when Value is on the allowed side of Bound, Bound
% itself included. One truth table serves both: each compares
% the pair the way round that puts the smaller term first.
at_least_t(Bound, Value, Truth) :-
    zcompare(Order, Bound, Value),
    within_truth(Order, Truth).

at_most_t(Bound, Value, Truth) :-
    zcompare(Order, Value, Bound),
    within_truth(Order, Truth).

within_truth(<, true).
within_truth(=, true).
within_truth(>, false).
