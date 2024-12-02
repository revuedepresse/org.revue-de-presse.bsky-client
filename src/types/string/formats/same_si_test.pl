:- module(same_si_test, [main/0]).

:- use_module('../../../assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module(same_si).

test(Spec) :-
    Spec = 'same_si:failing',
    assert(
        Spec,
        catch(same_si:same_si("%", "i"), E, true),
        error_must_be_same("%", "i"),
        E
    ).

test(Spec) :-
    Spec = 'same_si:succeeding',
    assert(
        Spec,
        (same_si:same_si("%", "%"), AreSame = true),
        are_same:true,
        are_same:AreSame
    ).

main :-
    Specs = [
        'same_si:failing',
        'same_si:succeeding'
    ],
    run_test_suite(same_si_test, Specs).

:- initialization(main).