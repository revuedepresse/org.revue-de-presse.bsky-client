:- module(did_identifier, [
    has_only_allowed_chars/1,
    is_valid_did_identifier/1,
    must_have_valid_identifier_format/1,
    must_have_valid_method_format/1,
    must_have_valid_percent_encoding/1
]).

:- use_module(library(clpz)).
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
%
% - The URI is case-sensitive
% - Percent-sign (%) is used for "percent encoding" in the identifier section,
%   and must always be followed by two hex characters
% - Query (?) and fragment (#) sections are allowed in DID URIs, but not in DID identifiers.
%   In the context of atproto, the query and fragment parts are not allowed.
% - The URI starts with lowercase `did`:
is_valid_did_identifier(Subject) :-
    has_only_ascii_chars(Subject),
    has_only_allowed_chars(Subject),
    must_start_with(Subject, "did:"),
    must_have_valid_method_format(Subject),
    must_have_valid_identifier_format(Subject),
    must_be_supported_method(Subject).

% The entire URI is made up of a subset of ASCII, containing letters (`A-Z`, `a-z`),
% digits (`0-9`), period, underscore, colon, percent sign, or hyphen (`._:%-`)
%
% has_only_allowed_chars(+Subject).
has_only_allowed_chars(Subject) :-
    \+  maplist(did_identifier:must_be_allowed_char, Subject)
    ->  throw(type_error('Subject must contain only allowed characters.'))
    ;   true.

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
    ;   member(Code, [ColonCode,DotCode,HyphenCode,PercentSignCode,UnderscoreCode]) ).

% In the context of atproto, implementations do not need to validate percent encoding.
% The percent symbol is allowed in DID identifier segments,
% but the identifier should not end in a percent symbol.
% - The remainder of the URI (the identifier) may contain any of the above-allowed ASCII characters,
%   except for percent-sign (`%`)
% - The URI (and thus the remaining identifier) may not end in `:`.
%
% must_have_valid_identifier_format(+Subject).
must_have_valid_identifier_format(Subject) :-
    split_subject(Subject, ':', Segments),
    length(Segments, SegmentsLength),

    (   SegmentsLength #< 3
    ->  throw(error_not_enough_segments)
    ;   true ),

    (   SegmentsLength #> 3
    ->  throw(error_too_many_segments)
    ;   true ),

    nth0(2, Segments, Identifier),

    (   \+ has_only_allowed_chars(Identifier)
    ->  throw(error_identifier_must_have_allowed_chars)
    ;   true ),

    catch(
        must_not_end_with(Subject, '%'),
        _E,
        throw(error_identifier_must_not_end_with('%'))
    ),

    must_have_valid_percent_encoding(Identifier).

must_have_at_least_n_characters(N, Subject) :-
    length(Subject, NChars),
    NChars #< N
    ->  throw(error_must_have_at_least_n_characters(N))
    ;   true.

must_have_valid_percent_encoding(Subject) :-
    split_subject(Subject, '%', Segments),

    Subject = [FirstChar|_Rest],

    (   catch(same_si(FirstChar, '%'), _E, fail)
    ->  maplist(must_have_at_least_n_characters(2), Segments)
    ;   Segments = [_|SegmentsMinusFirst],
        maplist(must_have_at_least_n_characters(2), SegmentsMinusFirst) ),

    (   catch(same_si(FirstChar, '%'), _E, fail)
    ->  maplist(must_be_valid_percent_encoding, Segments)
    ;   Segments = [_|SegmentsMinusFirst],
        maplist(must_be_valid_percent_encoding, SegmentsMinusFirst) ).

% See [Percent-encoding](https://developer.mozilla.org/en-US/docs/Glossary/Percent-encoding)
%
% must_be_valid_percent_encoding(+Segment).
must_be_valid_percent_encoding(Segment) :-
    length(Segment, N),
    (   N #< 2
    ->  throw(error_percent_encoding_must_have_two_hex_chars)
    ;   true ),

    Segment = [FirstChar|[SecondChar|_]],

    append([[FirstChar],[SecondChar]], Code),

    \+ member(Code, [
        "3A", % `:`
        "2F", % `/`
        "3F", % `?`
        "23", % `#`
        "5B", % `[`
        "5D", % `]`
        "2A", % `*`
        "2B", % `+`
        "2C", % `,`
        "3B", % `;`
        "3D", % `=`
        "3a", % `:`
        "2f", % `/`
        "3f", % `?`
        "23", % `#`
        "5b", % `[`
        "5d", % `]`
        "2a", % `*`
        "2b", % `+`
        "2c", % `,`
        "3b", % `;`
        "3d", % `=`
        "21", % `!`
        "24", % `$`
        "25", % `%`
        "26", % `&`
        "27", % `'`
        "28", % `(`
        "29", % `)`
        "20", % ` `
        "40"  % `@`
    ])
    ->  throw(error_invalid_percent_encoding)
    ;   true.

must_not_be_empty(Subject) :-
    length(Subject, N),
    \+ dif_si(N, 0)
    ->  throw(error_must_not_be_empty)
    ;   true.

% The method segment is one or more lowercase letters (`a-z`),
% followed by `:`
%
% must_have_valid_method_format(+Subject).
must_have_valid_method_format(Subject) :-
    split_subject(Subject, ':', Segments),
    length(Segments, SegmentsLength),

    maplist(must_not_be_empty, Segments),

    (   SegmentsLength #< 3
    ->  throw(error_not_enough_segments)
    ;   true ),

    nth0(1, Segments, Method),

    (   \+ must_have_lowercase_alpha_chars_only(Method)
    ->  throw(error_method_must_be_lowercase_alpha_only)
    ;   true ).

% Currently, atproto supports two DID methods:
%
% `did:web`, which is a W3C standard based on HTTPS (and DNS).
% The identifier section is a hostname. This method is supported in atproto
% to provide an independent alternative to did:plc.
% The method is inherently tied to the domain name used,
% and does not provide a mechanism for migration or recovering from loss of control of the domain name.
% In the context of atproto, only hostname-level did:web DIDs are supported:
% path-based DIDs are not supported.
% The same restrictions on top-level domains that apply to handles (eg, no .arpa) also apply to did:web domains.
% The special localhost hostname is allowed, but only in testing and development environments.
% Port numbers (with separating colon hex-encoded) are only allowed for localhost,
% and only in testing and development.
% `did:plc`, which is a novel DID method developed by Bluesky.
% See the [did-method-plc](https://github.com/did-method-plc/did-method-plc) GitHub repository for details.

% must_be_supported_method(+Subject).
must_be_supported_method(Subject) :-
    split_subject(Subject, ':', Segments),
    length(Segments, SegmentsLength),

    (   SegmentsLength #< 3
    ->  throw(error_not_enough_segments)
    ;   true ),

    nth0(1, Segments, Method),

    SupportedMethods = ["web","plc"],
    (   \+ member(Method, SupportedMethods)
    ->  throw(error_method_must_be_supported)
    ;   true ).

% must_have_lowercase_alpha_chars_only(+Subject).
must_have_lowercase_alpha_chars_only(Subject) :-
    must_be_ground(Subject),
    maplist(must_be_lowercase_alpha, Subject).
