:- module(clean_text, [
    clean_text/2,
    repair_mojibake/2
]).

:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(reif)).

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

Written in the reified style src/link_shortener.pl sets out: conditions
are predicates whose last argument is a truth value, chosen with `if_/3`,
and a read that may not match hands back a tagged term (`escape/2`,
`hex/2`, `digit/2`, `inner/1`, `found/1`, `none`) the caller dispatches
on by pattern. No cuts, no if-then-else, no negation as failure: each
stage stays a relation, which is what lets it be proven on its own.

Every stage walks the text once and rebuilds it, so a stage that matches
nothing is the identity on its input.
*/

% -- reified character primitives ------------------------------------------

%% code_in_t(+Low, +High, +Value, -Truth)
%
% `true` when `Low =< Value =< High`. Characters are only ever spoken
% about by code here: several of the ones that matter (`-`, `,`, `<`,
% `/`) are operator atoms Scryer will not read in argument position even
% quoted, so comparing codes is the only shape that works everywhere.
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

code_t(Expected, Char, Truth) :-
    char_code(Char, Code),
    =(Expected, Code, Truth).

backslash_t(Char, Truth) :- code_t(0x5C, Char, Truth).

quote_t(Char, Truth) :- code_t(0x22, Char, Truth).

marker_x_t(Char, Truth) :- code_t(0x78, Char, Truth).

%% first_found(+Key, +Pairs, -Result)
%
% `found(Value)` for the first `Key-Value` pair whose key matches, else
% `none`.
first_found(_, [], none).
first_found(Key, [K-V|Pairs], Result) :-
    if_(Key = K, Result = found(V), first_found(Key, Pairs, Result)).

%% repair_mojibake(+Chars, -Repaired)
%
% Detect the Ã/Â + UTF-8-continuation sniff; if present and every char is
% within Latin-1 (code =< 0xFF), reinterpret the codes as UTF-8 bytes.
% Anything else (no sniff, a char beyond Latin-1, a byte sequence the
% decoder refuses) leaves the input unchanged: the text was not mojibake
% after all, and a half-repair would be worse than none.
repair_mojibake([], []).
repair_mojibake([C|Cs], Out) :-
    if_(mojibake_t([C|Cs]),
        repaired_or_original([C|Cs], Out),
        Out = [C|Cs]).

% Both halves of the sniff have to hold: the sequence alone says the text
% may be mojibake, the Latin-1 bound says it can still be read back as
% bytes. A string holding anything above 0xFF is already valid UTF-8.
mojibake_t(Chars, Truth) :-
    if_(mojibake_sequence_t(Chars), all_latin1_t(Chars, Truth), Truth = false).

mojibake_sequence_t([], false).
mojibake_sequence_t([_], false).
mojibake_sequence_t([C1,C2|Cs], Truth) :-
    if_(mojibake_pair_t(C1, C2),
        Truth = true,
        mojibake_sequence_t([C2|Cs], Truth)).

mojibake_pair_t(Lead, Next, Truth) :-
    if_(latin1_lead_t(Lead), utf8_continuation_t(Next, Truth), Truth = false).

latin1_lead_t(Char, Truth) :-
    char_code(Char, Code),
    memberd_t(Code, [0xC2, 0xC3], Truth).

utf8_continuation_t(Char, Truth) :-
    char_code(Char, Code),
    code_in_t(0x80, 0xBF, Code, Truth).

all_latin1_t([], true).
all_latin1_t([C|Cs], Truth) :-
    if_(latin1_t(C), all_latin1_t(Cs, Truth), Truth = false).

latin1_t(Char, Truth) :-
    char_code(Char, Code),
    code_in_t(0, 0xFF, Code, Truth).

repaired_or_original(Chars, Out) :-
    chars_to_codes(Chars, Bytes),
    utf8_decoding(Bytes, Decoding),
    decoded_or_original(Decoding, Chars, Out).

% The decoder may refuse these bytes either way round - by failing or by
% throwing - and neither is an error here, so both are read as "not
% mojibake": findall/3 turns a refusal into an empty list, catch/3 turns
% a throw into one.
utf8_decoding(Bytes, Decoding) :-
    catch(findall(Chars, chars_utf8bytes(Chars, Bytes), Decodings),
          _,
          Decodings = []),
    first_decoding(Decodings, Decoding).

first_decoding([], undecodable).
first_decoding([Chars|_], decoded(Chars)).

decoded_or_original(decoded(Decoded), _, Decoded).
decoded_or_original(undecodable, Chars, Chars).

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
clean_text([], []).
clean_text([C|Cs], Output) :-
    repair_mojibake([C|Cs], S1),
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
strip_surrounding_quotes(Chars, Out) :-
    quoted_inner(Chars, Quoted),
    unquoted(Quoted, Chars, Out).

% `inner(Inner)` when a leading quote is answered by a trailing one,
% `none` otherwise - a lone quote at either end belongs to the text.
quoted_inner([], none).
quoted_inner([C|Cs], Quoted) :-
    if_(quote_t(C), closing_quote(Cs, Quoted), Quoted = none).

closing_quote([], none).
closing_quote([C|Cs], Quoted) :-
    reverse([C|Cs], [Last|ReversedInner]),
    if_(quote_t(Last),
        ( reverse(ReversedInner, Inner), Quoted = inner(Inner) ),
        Quoted = none).

unquoted(inner(Inner), _, Inner).
unquoted(none, Chars, Chars).

%% decode_backslash_letter(+Chars, -Out)
%
% Replaces the three "literal backslash + letter" escapes the JSON layer
% leaks: \n -> LF, \' -> ', \" -> ". A trailing lone backslash is left
% for strip_backslashes/2 to clean up.
decode_backslash_letter([], []).
decode_backslash_letter([C|Cs], Out) :-
    if_(backslash_t(C),
        ( backslash_escape(Cs, Escaped), escaped_step(Escaped, C, Cs, Out) ),
        ( Out = [C|Tail], decode_backslash_letter(Cs, Tail) )).

% Escape char code - decoded char code. Written as codes because the
% character written is not the character meant: `n` stands for a line
% feed, the two quotes for themselves.
backslash_escapes([0x6E-0x0A, 0x27-0x27, 0x22-0x22]).

backslash_escape([], none).
backslash_escape([C|_], Escaped) :-
    char_code(C, Code),
    backslash_escapes(Escapes),
    first_found(Code, Escapes, Escaped).

% An escape consumes the backslash and the char after it; anything else
% keeps the backslash and resumes at that char, which is where a
% trailing lone backslash ends up.
escaped_step(found(Code), _, [_|Cs], [Decoded|Tail]) :-
    char_code(Decoded, Code),
    decode_backslash_letter(Cs, Tail).
escaped_step(none, Backslash, Cs, [Backslash|Tail]) :-
    decode_backslash_letter(Cs, Tail).

%% decode_hex_4digit(+Chars, -Out)
%
% \xHHHH followed by an optional `\`. NBSP-family codepoints (0xA0,
% 0x2007, 0x202F) become a regular space; controls (< 0x20 or in the
% 0x7F..0x9F band) are dropped; everything else becomes its character.
decode_hex_4digit([], []).
decode_hex_4digit([C|Cs], Out) :-
    if_(backslash_t(C),
        ( x_escape(four_hex, Cs, Escape), hex_4digit_step(Escape, C, Cs, Out) ),
        ( Out = [C|Tail], decode_hex_4digit(Cs, Tail) )).

hex_4digit_step(escape(Code, Rest), _, _, Out) :-
    decoded_4digit_chars(Code, Chars),
    append(Chars, Tail, Out),
    decode_hex_4digit(Rest, Tail).
hex_4digit_step(none, Backslash, Cs, [Backslash|Tail]) :-
    decode_hex_4digit(Cs, Tail).

decoded_4digit_chars(Code, Chars) :-
    if_(memberd_t(Code, [0xA0, 0x2007, 0x202F]),
        Chars = [' '],
        if_(control_code_t(Code),
            Chars = [],
            char_of_code(Code, Chars))).

% The NBSP family stands where a space was meant; a control character
% stands for nothing a reader can see, so it goes rather than becoming a
% glyph the renderer has to guess at.
control_code_t(Code, Truth) :-
    if_(code_in_t(0, 0x1F, Code),
        Truth = true,
        code_in_t(0x7F, 0x9F, Code, Truth)).

% A surrogate half, or any other code char_code/2 refuses, has no
% character to become: dropped, like the controls above.
char_of_code(Code, Chars) :-
    catch(findall(Char, char_code(Char, Code), Found), _, Found = []),
    first_char(Found, Chars).

first_char([], []).
first_char([Char|_], [Char]).

%% decode_hex_2digit_x(+Chars, -Out)
%
% \xHH followed by an optional `\`. NBSP (0xA0) -> space; printable
% ASCII (0x20..0x7E) -> char; everything else dropped.
decode_hex_2digit_x([], []).
decode_hex_2digit_x([C|Cs], Out) :-
    if_(backslash_t(C),
        ( x_escape(two_hex, Cs, Escape), hex_2digit_x_step(Escape, C, Cs, Out) ),
        ( Out = [C|Tail], decode_hex_2digit_x(Cs, Tail) )).

hex_2digit_x_step(escape(Code, Rest), _, _, Out) :-
    decoded_2digit_chars(Code, Chars),
    append(Chars, Tail, Out),
    decode_hex_2digit_x(Rest, Tail).
hex_2digit_x_step(none, Backslash, Cs, [Backslash|Tail]) :-
    decode_hex_2digit_x(Cs, Tail).

%% decode_hex_2digit_bare(+Chars, -Out)
%
% Same as the `\x` form but without the `x`. CSS-style: `\2f\` -> `/`.
decode_hex_2digit_bare([], []).
decode_hex_2digit_bare([C|Cs], Out) :-
    if_(backslash_t(C),
        ( bare_escape(Cs, Escape), hex_2digit_bare_step(Escape, C, Cs, Out) ),
        ( Out = [C|Tail], decode_hex_2digit_bare(Cs, Tail) )).

hex_2digit_bare_step(escape(Code, Rest), _, _, Out) :-
    decoded_2digit_chars(Code, Chars),
    append(Chars, Tail, Out),
    decode_hex_2digit_bare(Rest, Tail).
hex_2digit_bare_step(none, Backslash, Cs, [Backslash|Tail]) :-
    decode_hex_2digit_bare(Cs, Tail).

decoded_2digit_chars(Code, Chars) :-
    if_(Code = 0xA0,
        Chars = [' '],
        if_(code_in_t(0x20, 0x7E, Code),
            ( char_code(Char, Code), Chars = [Char] ),
            Chars = [])).

%% x_escape(:Reader, +Chars, -Escape)
%
% `escape(Code, Rest)` when Chars opens with `x` followed by the digits
% Reader accepts, `none` otherwise. The caller has already consumed the
% backslash; Rest has the closing `\` these escapes may carry consumed
% too, since it belongs to the escape and not to the text.
x_escape(_, [], none).
x_escape(Reader, [C|Cs], Escape) :-
    if_(marker_x_t(C),
        ( call(Reader, Cs, Hex), hex_escape(Hex, Escape) ),
        Escape = none).

%% bare_escape(+Chars, -Escape)
%
% The same read without the `x` marker. `x` is not a hex digit, so a `\x`
% form can never be mistaken for one of these.
bare_escape(Chars, Escape) :-
    two_hex(Chars, Hex),
    hex_escape(Hex, Escape).

hex_escape(none, none).
hex_escape(hex(Code, Rest0), escape(Code, Rest)) :-
    drop_optional_backslash(Rest0, Rest).

%% four_hex(+Chars, -Result) and two_hex(+Chars, -Result)
%
% `hex(Code, Rest)` when that many hex digits open Chars, else `none`.
% Written as two then two so a half-read four-digit escape says `none`
% rather than silently standing for its first two digits - which is the
% whole reason the 4-hex stage runs before the 2-hex ones.
four_hex(Chars, Result) :-
    two_hex(Chars, High),
    four_hex_high(High, Result).

four_hex_high(none, none).
four_hex_high(hex(High, Rest), Result) :-
    two_hex(Rest, Low),
    four_hex_low(Low, High, Result).

four_hex_low(none, _, none).
four_hex_low(hex(Low, Rest), High, hex(Code, Rest)) :-
    Code #= High * 0x100 + Low.

two_hex(Chars, Result) :-
    hex_head(Chars, First),
    two_hex_first(First, Result).

two_hex_first(none, none).
two_hex_first(digit(High, Rest), Result) :-
    hex_head(Rest, Second),
    two_hex_second(Second, High, Result).

two_hex_second(none, _, none).
two_hex_second(digit(Low, Rest), High, hex(Code, Rest)) :-
    Code #= High * 0x10 + Low.

%% hex_head(+Chars, -Result)
%
% `digit(Value, Rest)` for a leading hex digit, else `none`.
hex_head([], none).
hex_head([C|Cs], Result) :-
    char_code(C, Code),
    hex_digit_value(Code, Value),
    hex_head_value(Value, Cs, Result).

hex_head_value(none, _, none).
hex_head_value(digit_value(Value), Cs, digit(Value, Cs)).

hex_digit_value(Code, Result) :-
    if_(code_in_t(0x30, 0x39, Code),                % 0-9
        ( Value #= Code - 0x30, Result = digit_value(Value) ),
        if_(code_in_t(0x61, 0x66, Code),            % a-f
            ( Value #= Code - 0x57, Result = digit_value(Value) ),
            if_(code_in_t(0x41, 0x46, Code),        % A-F
                ( Value #= Code - 0x37, Result = digit_value(Value) ),
                Result = none))).

%% strip_decimal_4digit(+Chars, -Out)
%
% `\NNNN` (four decimal digits) optionally followed by `\` -> dropped.
strip_decimal_4digit([], []).
strip_decimal_4digit([C|Cs], Out) :-
    if_(backslash_t(C),
        ( decimal_run([_,_,_,_], Cs, Run), decimal_step(Run, C, Cs, Out) ),
        ( Out = [C|Tail], strip_decimal_4digit(Cs, Tail) )).

decimal_step(decimals(Rest0), _, _, Out) :-
    drop_optional_backslash(Rest0, Rest),
    strip_decimal_4digit(Rest, Out).
decimal_step(none, Backslash, Cs, [Backslash|Tail]) :-
    strip_decimal_4digit(Cs, Tail).

%% decimal_run(+Places, +Chars, -Result)
%
% `decimals(Rest)` when Chars opens with as many decimal digits as
% Places has elements, else `none`. Places counts down as a list rather
% than as an integer: a list bottoms out on `[]`, which makes the two
% clauses mutually exclusive by their heads alone.
decimal_run([], Chars, decimals(Chars)).
decimal_run([_|Places], Chars, Result) :-
    decimal_head(Chars, Head),
    decimal_run_head(Head, Places, Result).

decimal_run_head(none, _, none).
decimal_run_head(digit(Rest), Places, Result) :-
    decimal_run(Places, Rest, Result).

decimal_head([], none).
decimal_head([C|Cs], Result) :-
    char_code(C, Code),
    if_(code_in_t(0x30, 0x39, Code), Result = digit(Cs), Result = none).

%% strip_zero_width(+Chars, -Out)
%
% Drops invisible joiners and variation selectors. Set matches the JS
% character class: U+200B, U+200C, U+200D, U+2060, U+FE0E, U+FE0F.
strip_zero_width([], []).
strip_zero_width([C|Cs], Out) :-
    if_(zero_width_t(C),
        strip_zero_width(Cs, Out),
        ( Out = [C|Tail], strip_zero_width(Cs, Tail) )).

zero_width_t(Char, Truth) :-
    char_code(Char, Code),
    memberd_t(Code, [0x200B, 0x200C, 0x200D, 0x2060, 0xFE0E, 0xFE0F], Truth).

%% strip_backslashes(+Chars, -Out)
%
% Runs last in the pipeline: every escape has had its chance to claim its
% backslash, so what is left stands for nothing.
strip_backslashes([], []).
strip_backslashes([C|Cs], Out) :-
    if_(backslash_t(C),
        strip_backslashes(Cs, Out),
        ( Out = [C|Tail], strip_backslashes(Cs, Tail) )).

%% collapse_whitespace(+Chars, -Out)
%
% A run of two or more space/tab chars collapses to one space. Mirrors
% the JS regex `[ \t]{2,}` -> ' '; does not touch newlines, since the
% rendering side decides what to do with them.
collapse_whitespace([], []).
collapse_whitespace([C|Cs], Out) :-
    if_(blank_run_t([C|Cs]),
        ( Out = [' '|Tail],
          skip_blanks(Cs, Rest),
          collapse_whitespace(Rest, Tail) ),
        ( Out = [C|Tail], collapse_whitespace(Cs, Tail) )).

% Two blanks in a row say nothing one space does not. A single blank is
% left exactly as it stands, tab included: on its own it may be what the
% author typed.
blank_run_t([], false).
blank_run_t([C|Cs], Truth) :-
    if_(blank_t(C), leading_blank_t(Cs, Truth), Truth = false).

leading_blank_t([], false).
leading_blank_t([C|_], Truth) :- blank_t(C, Truth).

skip_blanks([], []).
skip_blanks([C|Cs], Out) :-
    if_(blank_t(C), skip_blanks(Cs, Out), Out = [C|Cs]).

blank_t(Char, Truth) :-
    char_code(Char, Code),
    memberd_t(Code, [0x20, 0x09], Truth).

%% trim_whitespace(+Chars, -Out)
%
% Drop leading and trailing whitespace (space, tab, CR, LF). The trailing
% end is the leading end of the reverse, so one relation does both.
trim_whitespace(Chars, Out) :-
    drop_leading_whitespace(Chars, Mid),
    reverse(Mid, Reversed),
    drop_leading_whitespace(Reversed, ReversedTrimmed),
    reverse(ReversedTrimmed, Out).

drop_leading_whitespace([], []).
drop_leading_whitespace([C|Cs], Out) :-
    if_(whitespace_t(C), drop_leading_whitespace(Cs, Out), Out = [C|Cs]).

whitespace_t(Char, Truth) :-
    char_code(Char, Code),
    memberd_t(Code, [0x20, 0x09, 0x0A, 0x0D], Truth).

%% drop_optional_backslash(+Chars, -Out)
%
% Consume one `\` if present; otherwise leave the list alone. The hex
% forms are written both ways upstream (`\xa0` and `\xa0\`), so the
% closing backslash is part of the escape when it is there.
drop_optional_backslash([], []).
drop_optional_backslash([C|Cs], Out) :-
    if_(backslash_t(C), Out = Cs, Out = [C|Cs]).
