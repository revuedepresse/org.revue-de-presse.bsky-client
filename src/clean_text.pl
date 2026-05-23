:- module(clean_text, [
    clean_text/2,
    repair_mojibake/2
]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(si)).

/**
Strips upstream encoding artefacts from raw text before it crosses the
repository write boundary.

Ported from next/lib/cleanText.ts so the same normalisation runs once at
ingest instead of being re-applied by every downstream client. The
behaviour is pinned by tests/clean_text_test.pl, which mirrors the cases
in next/lib/cleanText.spec.ts and social/linkedin/test/cleanText.spec.ts.

Two predicates are exported:

  * `repair_mojibake/2` - undoes the "UTF-8 bytes mistakenly decoded as
    Latin-1" pattern (e.g. `CafÃ©` -> `Café`). Aborts when any char is
    outside Latin-1, since that means the string is already valid UTF-8.

  * `clean_text/2` - full pipeline: mojibake repair, surrounding-quote
    strip, escape decoding (`\n`, `\'`, `\"`, `\xNNNN\?`, `\xNN\?`,
    `\NN\?`), `\NNNN\?` strip, zero-width Unicode strip, bare-backslash
    strip, internal-whitespace collapse, trim.

Both predicates take and return Prolog char lists (use atom_chars/2 to
adapt from atoms and back).
*/

%% repair_mojibake(+Chars, -Repaired)
%
% Detect the Ã/Â + UTF-8-continuation sniff; if present and every char is
% within Latin-1 (code =< 0xFF), reinterpret the codes as UTF-8 bytes.
% Any failure (invalid UTF-8 sequence, non-Latin-1 char, no sniff) leaves
% the input unchanged.
repair_mojibake([], []).
repair_mojibake([H|T], Out) :-
    Chars = [H|T],
    (   has_mojibake_sniff(Chars),
        all_latin1(Chars),
        chars_to_codes(Chars, Bytes),
        catch(chars_utf8bytes(Decoded, Bytes), _, fail)
    ->  Out = Decoded
    ;   Out = Chars
    ).

has_mojibake_sniff([C1,C2|_]) :-
    char_code(C1, K1),
    ( K1 =:= 0xC2 ; K1 =:= 0xC3 ),
    char_code(C2, K2),
    K2 >= 0x80, K2 =< 0xBF, !.
has_mojibake_sniff([_,B|Rest]) :-
    has_mojibake_sniff([B|Rest]).

all_latin1([]).
all_latin1([C|Cs]) :-
    char_code(C, K),
    K =< 0xFF,
    all_latin1(Cs).

chars_to_codes([], []).
chars_to_codes([C|Cs], [K|Ks]) :-
    char_code(C, K),
    chars_to_codes(Cs, Ks).

%% clean_text(+Chars, -Cleaned)
%
% Runs the full normalisation pipeline. Order matters: 4-hex must run
% before 2-hex (otherwise `\x202f` is consumed as `\x20` plus literal
% `2f`); strip-backslashes must run last so escape decoders can still
% see their leading `\`.
clean_text([], []) :- !.
clean_text(Input, Output) :-
    repair_mojibake(Input, S1),
    strip_surrounding_quotes(S1, S2),
    decode_backslash_letter(S2, S3),
    decode_hex_4digit(S3, S4),
    decode_hex_2digit_x(S4, S5),
    decode_hex_2digit_bare(S5, S6),
    strip_decimal_4digit(S6, S7),
    strip_zero_width(S7, S8),
    strip_backslashes(S8, S9),
    collapse_whitespace(S9, S10),
    trim_whitespace(S10, Output).

%% strip_surrounding_quotes(+Chars, -Out)
%
% Drop a single pair of wrapping ASCII double quotes. The JS version
% only handles the symmetric case; we mirror that.
strip_surrounding_quotes(['"'|Rest], Inner) :-
    append(Inner, ['"'], Rest), !.
strip_surrounding_quotes(Chars, Chars).

%% decode_backslash_letter(+Chars, -Out)
%
% Replaces the three "literal backslash + letter" escapes the JSON layer
% leaks: \n -> LF, \' -> ', \" -> ". A trailing lone backslash is left
% for strip_backslashes/2 to clean up.
decode_backslash_letter([], []).
decode_backslash_letter(['\\','n'|Rest], ['\n'|Out]) :- !,
    decode_backslash_letter(Rest, Out).
decode_backslash_letter(['\\','\''|Rest], ['\''|Out]) :- !,
    decode_backslash_letter(Rest, Out).
decode_backslash_letter(['\\','"'|Rest], ['"'|Out]) :- !,
    decode_backslash_letter(Rest, Out).
decode_backslash_letter([C|Rest], [C|Out]) :-
    decode_backslash_letter(Rest, Out).

%% decode_hex_4digit(+Chars, -Out)
%
% \xHHHH followed by an optional `\`. NBSP-family codepoints (0xA0,
% 0x2007, 0x202F) become a regular space; controls (< 0x20 or in the
% 0x7F..0x9F band) are dropped; everything else becomes its character.
decode_hex_4digit([], []).
decode_hex_4digit(['\\','x',H1,H2,H3,H4|Rest0], Out) :-
    hex_digit(H1, V1),
    hex_digit(H2, V2),
    hex_digit(H3, V3),
    hex_digit(H4, V4),
    !,
    Code is V1 * 0x1000 + V2 * 0x100 + V3 * 0x10 + V4,
    drop_optional_backslash(Rest0, Rest),
    decoded_4digit_chars(Code, Chars),
    append(Chars, OutRest, Out),
    decode_hex_4digit(Rest, OutRest).
decode_hex_4digit([C|Rest], [C|Out]) :-
    decode_hex_4digit(Rest, Out).

decoded_4digit_chars(0xA0, [' ']) :- !.
decoded_4digit_chars(0x2007, [' ']) :- !.
decoded_4digit_chars(0x202F, [' ']) :- !.
decoded_4digit_chars(Code, []) :- Code < 0x20, !.
decoded_4digit_chars(Code, []) :- Code >= 0x7F, Code < 0xA0, !.
decoded_4digit_chars(Code, [Char]) :-
    catch(char_code(Char, Code), _, fail), !.
decoded_4digit_chars(_, []).

%% decode_hex_2digit_x(+Chars, -Out)
%
% \xHH followed by an optional `\`. NBSP (0xA0) -> space; printable
% ASCII (0x20..0x7E) -> char; everything else dropped.
decode_hex_2digit_x([], []).
decode_hex_2digit_x(['\\','x',H1,H2|Rest0], Out) :-
    hex_digit(H1, V1),
    hex_digit(H2, V2),
    !,
    Code is V1 * 0x10 + V2,
    drop_optional_backslash(Rest0, Rest),
    decoded_2digit_chars(Code, Chars),
    append(Chars, OutRest, Out),
    decode_hex_2digit_x(Rest, OutRest).
decode_hex_2digit_x([C|Rest], [C|Out]) :-
    decode_hex_2digit_x(Rest, Out).

%% decode_hex_2digit_bare(+Chars, -Out)
%
% Same as the `\x` form but without the `x`. CSS-style: `\2f\` -> `/`.
decode_hex_2digit_bare([], []).
decode_hex_2digit_bare(['\\',H1,H2|Rest0], Out) :-
    hex_digit(H1, V1),
    hex_digit(H2, V2),
    !,
    Code is V1 * 0x10 + V2,
    drop_optional_backslash(Rest0, Rest),
    decoded_2digit_chars(Code, Chars),
    append(Chars, OutRest, Out),
    decode_hex_2digit_bare(Rest, OutRest).
decode_hex_2digit_bare([C|Rest], [C|Out]) :-
    decode_hex_2digit_bare(Rest, Out).

decoded_2digit_chars(0xA0, [' ']) :- !.
decoded_2digit_chars(Code, [Char]) :-
    Code >= 0x20, Code < 0x7F, !,
    char_code(Char, Code).
decoded_2digit_chars(_, []).

%% strip_decimal_4digit(+Chars, -Out)
%
% `\NNNN` (four decimal digits) optionally followed by `\` -> dropped.
strip_decimal_4digit([], []).
strip_decimal_4digit(['\\',D1,D2,D3,D4|Rest0], Out) :-
    decimal_digit(D1),
    decimal_digit(D2),
    decimal_digit(D3),
    decimal_digit(D4),
    !,
    drop_optional_backslash(Rest0, Rest),
    strip_decimal_4digit(Rest, Out).
strip_decimal_4digit([C|Rest], [C|Out]) :-
    strip_decimal_4digit(Rest, Out).

%% strip_zero_width(+Chars, -Out)
%
% Drops invisible joiners and variation selectors. Set matches the JS
% character class: U+200B, U+200C, U+200D, U+2060, U+FE0E, U+FE0F.
strip_zero_width([], []).
strip_zero_width([C|Rest], Out) :-
    char_code(C, K),
    is_zero_width(K), !,
    strip_zero_width(Rest, Out).
strip_zero_width([C|Rest], [C|Out]) :-
    strip_zero_width(Rest, Out).

is_zero_width(0x200B).
is_zero_width(0x200C).
is_zero_width(0x200D).
is_zero_width(0x2060).
is_zero_width(0xFE0E).
is_zero_width(0xFE0F).

%% strip_backslashes(+Chars, -Out)
strip_backslashes([], []).
strip_backslashes(['\\'|Rest], Out) :- !,
    strip_backslashes(Rest, Out).
strip_backslashes([C|Rest], [C|Out]) :-
    strip_backslashes(Rest, Out).

%% collapse_whitespace(+Chars, -Out)
%
% A run of two or more space/tab chars collapses to one space. Mirrors
% the JS regex `[ \t]{2,}` -> ' '; does not touch newlines, since the
% rendering side decides what to do with them.
collapse_whitespace([], []).
collapse_whitespace([C|Rest], [' '|Out]) :-
    is_space_or_tab(C),
    Rest = [C2|_],
    is_space_or_tab(C2), !,
    skip_space_tab(Rest, Rest1),
    collapse_whitespace(Rest1, Out).
collapse_whitespace([C|Rest], [C|Out]) :-
    collapse_whitespace(Rest, Out).

skip_space_tab([C|Rest], Out) :-
    is_space_or_tab(C), !,
    skip_space_tab(Rest, Out).
skip_space_tab(Chars, Chars).

is_space_or_tab(' ').
is_space_or_tab('\t').

%% trim_whitespace(+Chars, -Out)
%
% Drop leading and trailing whitespace (space, tab, CR, LF).
trim_whitespace(Chars, Out) :-
    drop_leading_ws(Chars, Mid),
    reverse(Mid, Rev),
    drop_leading_ws(Rev, RevTrimmed),
    reverse(RevTrimmed, Out).

drop_leading_ws([C|Rest], Out) :-
    is_ws(C), !,
    drop_leading_ws(Rest, Out).
drop_leading_ws(Chars, Chars).

is_ws(' ').
is_ws('\t').
is_ws('\n').
is_ws('\r').

%% drop_optional_backslash(+Chars, -Out)
%
% Consume one `\` if present; otherwise leave the list alone.
drop_optional_backslash(['\\'|Rest], Rest) :- !.
drop_optional_backslash(Chars, Chars).

%% hex_digit(+Char, -Value)
hex_digit(C, V) :-
    char_code(C, K),
    hex_value(K, V).

hex_value(K, V) :- K >= 0x30, K =< 0x39, !, V is K - 0x30.
hex_value(K, V) :- K >= 0x61, K =< 0x66, !, V is K - 0x57.
hex_value(K, V) :- K >= 0x41, K =< 0x46, V is K - 0x37.

decimal_digit(C) :-
    char_code(C, K),
    K >= 0x30, K =< 0x39.
