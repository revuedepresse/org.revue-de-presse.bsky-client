:- module(clean_text_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).

:- use_module('../src/clean_text', [
    clean_text/2,
    repair_mojibake/2
]).

/*
Parity tests for src/clean_text.pl. Every case mirrors one from
next/lib/cleanText.spec.ts or social/linkedin/test/cleanText.spec.ts so
the Prolog ingest-side cleaner and the JS legacy-fallback cleaner stay
behaviour-compatible.

Run with:
  scryer-prolog ./tests/clean_text_test.pl -g run_test

DB-free and HTTP-free.
*/

% repair_mojibake -----------------------------------------------------------

cases_repair_mojibake([
    case("repairs Ã© -> é",                  "CafÃ©",                       "Café"),
    case("repairs Ã¨ -> è",                  "IsraÃ«l",                     "Israël"),
    case("repairs multiple sequences",       "L'HumanitÃ© attaquÃ©e",       "L'Humanité attaquée"),
    case("leaves valid UTF-8 untouched",     "Café déjà-vu",                "Café déjà-vu"),
    case("leaves plain ASCII untouched",     "hello world",                 "hello world"),
    case("returns empty string for empty",   "",                            "")
]).

% Cases involving codepoints > 0xFF (emoji): repair_mojibake must NOT touch
% these because at least one char is beyond Latin-1.
case_emoji_passthrough("🌷 CafÃ©", "🌷 CafÃ©").

% clean_text ---------------------------------------------------------------

cases_clean_text([
    case("empty -> empty",                                    "",                                ""),
    case("strips wrapping straight quotes",                   "\"Hello\"",                       "Hello"),
    case("decodes literal backslash-n into LF",               "line one\\nline two",             "line one\nline two"),
    case("decodes escaped quotes inside body",                "L\\'Espagne et l\\'Italie",       "L'Espagne et l'Italie"),
    case("decodes \\xa0\\ as space and collapses runs",       "1er\\xa0\\mai",                   "1er mai"),
    case("decodes 4-hex \\x202f\\ (NNBSP) as space",          "connue\\x202f\\: attaquer",       "connue : attaquer"),
    case("decodes 4-hex \\x2026 (HORIZONTAL ELLIPSIS)",       "voir aussi\\x2026",               "voir aussi…"),
    case("decodes CSS-style \\2f\\ as printable ASCII",       "A\\2f\\B",                        "A/B"),
    case("strips bare backslashes after other transforms",    "foo\\!bar",                       "foo!bar"),
    case("trims surrounding whitespace",                      "   hello   ",                     "hello"),
    case("repairs mojibake before stripping artefacts",       "\"CafÃ©\"",                       "Café"),
    case("does not confuse 2-digit \\xa0\\ with 4-digit",     "1er\\xa0\\mai",                   "1er mai")
]).

% Test runner --------------------------------------------------------------

run_test :-
    run_block("repair_mojibake", repair_mojibake, FailsR-TotalR),
    run_emoji_passthrough(FailsE-TotalE),
    FailsR2 is FailsR + FailsE,
    TotalR2 is TotalR + TotalE,
    run_block("clean_text", clean_text, FailsC-TotalC),
    TotalFails is FailsR2 + FailsC,
    TotalCount is TotalR2 + TotalC,
    Passed is TotalCount - TotalFails,
    (   TotalFails =:= 0
    ->  format("[OK] clean_text_test: ~w/~w cases passed~n", [Passed, TotalCount])
    ;   format("[KO] clean_text_test: ~w failures out of ~w cases~n", [TotalFails, TotalCount]),
        halt(1)
    ).

run_block(Label, Pred, Fails-Total) :-
    block_cases(Pred, Cases),
    run_cases(Pred, Cases, 0-0, Fails-Total),
    NumPassed is Total - Fails,
    format("  ~s: ~w/~w~n", [Label, NumPassed, Total]).

block_cases(repair_mojibake, Cases) :- cases_repair_mojibake(Cases).
block_cases(clean_text,     Cases) :- cases_clean_text(Cases).

run_cases(_, [], Acc, Acc).
run_cases(Pred, [case(Label, In, Want)|Rest], F0-T0, Final) :-
    T1 is T0 + 1,
    (   call(Pred, In, Got),
        Got == Want
    ->  F1 = F0
    ;   F1 is F0 + 1,
        ( catch(call(Pred, In, GotErr), Err, (GotErr = error(Err), true))
        -> format("    [KO] ~s~n       input:    ~q~n       expected: ~q~n       got:      ~q~n",
                  [Label, In, Want, GotErr])
        ;  format("    [KO] ~s (predicate failed)~n       input:    ~q~n       expected: ~q~n",
                  [Label, In, Want])
        )
    ),
    run_cases(Pred, Rest, F1-T1, Final).

run_emoji_passthrough(Fails-Total) :-
    Total = 1,
    case_emoji_passthrough(In, Want),
    (   repair_mojibake(In, Got),
        Got == Want
    ->  Fails = 0
    ;   Fails = 1,
        format("    [KO] emoji string passes through repair_mojibake unchanged~n"),
        format("       input:    ~q~n       expected: ~q~n", [In, Want])
    ).

:- initialization(run_test).
