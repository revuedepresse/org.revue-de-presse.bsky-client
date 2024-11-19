:- module(has_only_ascii_chars_test, [main/0]).

:- use_module(library(lists)).

:- use_module('../../../assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module(has_only_ascii_chars).

test(Spec) :-
    Spec = 'has_only_ascii_chars:has_only_ascii_chars',
    assert(
        Spec,
        catch(has_only_ascii_chars:has_only_ascii_chars("😅.test"), type_error(Message), true),
        'Subject must start with an ascii char.',
        Message
    ).

main :-
    Specs = [
        'has_only_ascii_chars:has_only_ascii_chars'
    ],
    run_test_suite(has_only_ascii_chars_test, Specs).

:- initialization(main).