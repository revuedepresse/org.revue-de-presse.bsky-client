:- module(at_identifier_test, [main/0]).

:- use_module(library(os)).
:- use_module(at_identifier).

:- use_module('../../../assert', [
    assert/4,
    run_test_suite/2
]).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:failing:1',
    assert(
        Spec,
        catch(
            at_identifier:is_valid_at_identifier("i%3A"),
            E,
            true
        ),
        error_must_have_two_labels_at_least,
        E
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:failing:2',
    assert(
        Spec,
        catch(
            at_identifier:is_valid_at_identifier("i%.3A"),
            E,
            true
        ),
        type_error('Subject must contain only allowed characters.'),
        E
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:failing:3',
    assert(
        Spec,
        catch(
            at_identifier:is_valid_at_identifier("i9.3A"),
            E,
            true
        ),
        error_must_not_start_with_digit,
        E
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:failing:4',
    assert(
        Spec,
        catch(
            at_identifier:is_valid_at_identifier("i9.-3A"),
            E,
            true
        ),
        error_must_not_start_with(-),
        E
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:failing:5',
    assert(
        Spec,
        catch(
            at_identifier:is_valid_at_identifier("i9.3A-"),
            E,
            true
        ),
        error_must_not_end_with(-),
        E
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:succeeding:1',
    assert(
        Spec,
        (at_identifier:is_valid_at_identifier("i9.A-3"), IsValidAtIdentifier = true),
        is_valid_at_identifier:true,
        is_valid_at_identifier:IsValidAtIdentifier
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:failing:6',
    assert(
        Spec,
        catch(
            at_identifier:is_valid_at_identifier("did:"),
            E,
            true
        ),
        'error_must_not_be_empty',
        E
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:failing:7',
    assert(
        Spec,
        catch(
            at_identifier:is_valid_at_identifier("did:method"),
            E,
            true
        ),
        'error_not_enough_segments',
        E
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:failing:8',
    assert(
        Spec,
        catch(
            at_identifier:is_valid_at_identifier("did:method:id"),
            E,
            true
        ),
        'error_method_must_be_supported',
        E
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:failing:9',
    assert(
        Spec,
        catch(
            at_identifier:is_valid_at_identifier("did:web:i%10d"),
            E,
            true
        ),
        'error_invalid_percent_encoding',
        E
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:failing:10',
    assert(
        Spec,
        catch(
            at_identifier:is_valid_at_identifier("did:web:i%30d%"),
            E,
            true
        ),
        error_identifier_must_not_end_with('%'),
        E
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:failing:11',
    assert(
        Spec,
        catch(
            at_identifier:is_valid_at_identifier("did:web:i%3Ad:"),
            E,
            true
        ),
        error_must_not_be_empty,
        E
    ).

test(Spec) :-
    Spec = 'at_identifier:is_valid_at_identifier:succeeding:2',
    assert(
        Spec,
        (at_identifier:is_valid_at_identifier("did:web:i%3Ad_"), IsValidAtIdentifier = true),
        is_valid_at_identifier:true,
        is_valid_at_identifier:IsValidAtIdentifier
    ).

main :-
    setenv("ENVIRONMENT", "production"),
    Specs = [
        'at_identifier:is_valid_at_identifier:failing:1',
        'at_identifier:is_valid_at_identifier:failing:2',
        'at_identifier:is_valid_at_identifier:failing:3',
        'at_identifier:is_valid_at_identifier:failing:4',
        'at_identifier:is_valid_at_identifier:failing:5',
        'at_identifier:is_valid_at_identifier:succeeding:1',
        'at_identifier:is_valid_at_identifier:failing:6',
        'at_identifier:is_valid_at_identifier:failing:7',
        'at_identifier:is_valid_at_identifier:failing:8',
        'at_identifier:is_valid_at_identifier:failing:9',
        'at_identifier:is_valid_at_identifier:failing:10',
        'at_identifier:is_valid_at_identifier:failing:11',
        'at_identifier:is_valid_at_identifier:succeeding:2'
    ],
    run_test_suite(at_identifier_test, Specs).

:- initialization(main).