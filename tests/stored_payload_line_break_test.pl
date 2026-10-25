:- module(stored_payload_line_break_test, [main/0]).

:- use_module('../src/assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module('../src/serialization', [
    stored_payload_json_chars/2
]).

/*
A stored payload is the Prolog string literal of a JSON text, in which
jq writes line breaks as they are. stored_payload_json_chars/2 escapes
them before reading the literal back, and leaves every other character
as it is.
*/

test(Spec) :-
    Spec = 'stored_payload:line_breaks_between_tokens_are_read_back',
    stored_payload_json_chars("\"{\n  \\\"a\\\": 1\n}\"", JSONChars),
    assert(Spec, true, "{\n  \"a\": 1\n}", JSONChars).
test(Spec) :-
    Spec = 'stored_payload:a_payload_without_line_breaks_is_read_back',
    stored_payload_json_chars("\"{\\\"a\\\":\\\"b\\\\nc\\\"}\"", JSONChars),
    assert(Spec, true, "{\"a\":\"b\\nc\"}", JSONChars).

main :-
    Specs = [
        'stored_payload:line_breaks_between_tokens_are_read_back',
        'stored_payload:a_payload_without_line_breaks_is_read_back'
    ],
    run_test_suite(stored_payload_line_break_test, Specs).

:- initialization(main).
