:- module(did_identifier, [
    has_only_allowed_chars/1,
    is_valid_did_identifier/1,
    must_have_valid_identifier_format/1,
    must_have_valid_method_format/1,
    must_have_valid_percent_encoding/1
]).

:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(si)).
:- use_module('../../../memoize', [memoize_goal/2, memoized_goal/2]).
:- use_module('../../../logger', [log_info/1]).
:- use_module(has_only_ascii_chars, [has_only_ascii_chars/1]).
:- use_module(must_be_chars, [must_be_chars/1]).
:- use_module(must_be_ground, [must_be_ground/1]).
:- use_module(must_be_lowercase_alpha, [must_be_lowercase_alpha/1]).
:- use_module(must_not_end_with, [must_not_end_with/2]).
:- use_module(must_start_with, [must_start_with/2]).
:- use_module(same_si, [same_si/2]).
:- use_module(split_subject, [split_subject/3]).

% See [AT Protocol DID Identifier Syntax](https://atproto.com/specs/did#at-protocol-did-identifier-syntax)
is_valid_did_identifier(Subject) :-
    has_only_ascii_chars(Subject),
    has_only_allowed_chars(Subject),
    must_start_with(Subject, "did:"),
    must_have_valid_method_format(Subject),
    must_have_valid_identifier_format(Subject),
    must_be_supported_method(Subject).

% has_only_allowed_chars(+Subject).
has_only_allowed_chars(Subject) :-
    once(has_only_allowed_chars_check(Subject)).

has_only_allowed_chars_check(Subject) :-
    maplist(did_identifier:must_be_allowed_char, Subject).
has_only_allowed_chars_check(Subject) :-
    \+ maplist(did_identifier:must_be_allowed_char, Subject),
    throw(type_error('Subject must contain only allowed characters.')).

% must_be_allowed_char(+Char).
must_be_allowed_char(Char) :-
    char_code('A', AlphaStartCode),
    char_code('Z', AlphaEndCode),
    char_code('0', DigitStartCode),
    char_code('9', DigitEndCode),
    char_code(':', ColonCode),
    char_code('.', DotCode),
    char_code('-', HyphenCode),
    char_code('%', PercentSignCode),
    char_code('_', UnderscoreCode),
    char_code(Char, Code),
    (   must_be_lowercase_alpha(Char)
    ;   Code #>= AlphaStartCode, Code #=< AlphaEndCode
    ;   Code #>= DigitStartCode, Code #=< DigitEndCode
    ;   member(Code, [ColonCode,DotCode,HyphenCode,PercentSignCode,UnderscoreCode])
    ).

% must_have_valid_identifier_format(+Subject).
must_have_valid_identifier_format(Subject) :-
    split_subject(Subject, ':', Segments),
    length(Segments, SegmentsLength),
    once(assert_segments_at_least_3(SegmentsLength)),
    once(assert_segments_at_most_3(SegmentsLength)),
    nth0(2, Segments, Identifier),
    assert_identifier_chars(Identifier),
    catch(
        must_not_end_with(Subject, '%'),
        _E,
        throw(error_identifier_must_not_end_with('%'))
    ),
    must_have_valid_percent_encoding(Identifier).

assert_segments_at_least_3(N) :- N >= 3.
assert_segments_at_least_3(N) :- N < 3, throw(error_not_enough_segments).

assert_segments_at_most_3(N) :- N =< 3.
assert_segments_at_most_3(N) :- N > 3, throw(error_too_many_segments).

assert_identifier_chars(Identifier) :- once(assert_identifier_chars_check(Identifier)).

assert_identifier_chars_check(Identifier) :- has_only_allowed_chars(Identifier).
assert_identifier_chars_check(Identifier) :-
    \+ has_only_allowed_chars(Identifier),
    throw(error_identifier_must_have_allowed_chars).

must_have_at_least_n_characters(N, Subject) :-
    length(Subject, NChars),
    NChars #>= N.
must_have_at_least_n_characters(N, Subject) :-
    length(Subject, NChars),
    NChars #< N,
    throw(error_must_have_at_least_n_characters(N)).

must_have_valid_percent_encoding(Subject) :-
    split_subject(Subject, '%', Segments),
    Subject = [FirstChar|_Rest],
    apply_min_length(FirstChar, Segments),
    apply_encoding_validation(FirstChar, Segments).

apply_min_length(FirstChar, Segments) :-
    starts_with_percent(FirstChar),
    maplist(must_have_at_least_n_characters(2), Segments).
apply_min_length(FirstChar, Segments) :-
    \+ starts_with_percent(FirstChar),
    Segments = [_|SegmentsMinusFirst],
    maplist(must_have_at_least_n_characters(2), SegmentsMinusFirst).

apply_encoding_validation(FirstChar, Segments) :-
    starts_with_percent(FirstChar),
    maplist(must_be_valid_percent_encoding, Segments).
apply_encoding_validation(FirstChar, Segments) :-
    \+ starts_with_percent(FirstChar),
    Segments = [_|SegmentsMinusFirst],
    maplist(must_be_valid_percent_encoding, SegmentsMinusFirst).

starts_with_percent(FirstChar) :- catch(same_si(FirstChar, '%'), _E, fail).

% must_be_valid_percent_encoding(+Segment).
must_be_valid_percent_encoding(Segment) :-
    length(Segment, N),
    assert_percent_pair_length(N),
    Segment = [FirstChar|[SecondChar|_]],
    append([[FirstChar],[SecondChar]], Code),
    assert_percent_code_allowed(Code).

assert_percent_pair_length(N) :- N #>= 2.
assert_percent_pair_length(N) :-
    N #< 2,
    throw(error_percent_encoding_must_have_two_hex_chars).

allowed_percent_code("3A").
allowed_percent_code("2F").
allowed_percent_code("3F").
allowed_percent_code("23").
allowed_percent_code("5B").
allowed_percent_code("5D").
allowed_percent_code("2A").
allowed_percent_code("2B").
allowed_percent_code("2C").
allowed_percent_code("3B").
allowed_percent_code("3D").
allowed_percent_code("3a").
allowed_percent_code("2f").
allowed_percent_code("3f").
allowed_percent_code("5b").
allowed_percent_code("5d").
allowed_percent_code("2a").
allowed_percent_code("2b").
allowed_percent_code("2c").
allowed_percent_code("3b").
allowed_percent_code("3d").
allowed_percent_code("21").
allowed_percent_code("24").
allowed_percent_code("25").
allowed_percent_code("26").
allowed_percent_code("27").
allowed_percent_code("28").
allowed_percent_code("29").
allowed_percent_code("20").
allowed_percent_code("40").

assert_percent_code_allowed(Code) :- allowed_percent_code(Code).
assert_percent_code_allowed(Code) :-
    \+ allowed_percent_code(Code),
    throw(error_invalid_percent_encoding).

must_not_be_empty(Subject) :-
    length(Subject, N),
    dif_si(N, 0).
must_not_be_empty(Subject) :-
    length(Subject, N),
    \+ dif_si(N, 0),
    throw(error_must_not_be_empty).

% must_have_valid_method_format(+Subject).
must_have_valid_method_format(Subject) :-
    split_subject(Subject, ':', Segments),
    length(Segments, SegmentsLength),
    maplist(must_not_be_empty, Segments),
    assert_segments_at_least_3(SegmentsLength),
    nth0(1, Segments, Method),
    assert_method_lowercase_alpha(Method).

assert_method_lowercase_alpha(Method) :- must_have_lowercase_alpha_chars_only(Method).
assert_method_lowercase_alpha(Method) :-
    \+ must_have_lowercase_alpha_chars_only(Method),
    throw(error_method_must_be_lowercase_alpha_only).

% must_be_supported_method(+Subject).
must_be_supported_method(Subject) :-
    split_subject(Subject, ':', Segments),
    length(Segments, SegmentsLength),
    assert_segments_at_least_3(SegmentsLength),
    nth0(1, Segments, Method),
    assert_method_supported(Method).

supported_method("web").
supported_method("plc").

assert_method_supported(Method) :- supported_method(Method).
assert_method_supported(Method) :-
    \+ supported_method(Method),
    throw(error_method_must_be_supported).

% must_have_lowercase_alpha_chars_only(+Subject).
must_have_lowercase_alpha_chars_only(Subject) :-
    must_be_ground(Subject),
    maplist(must_be_lowercase_alpha, Subject).
