:- module(did_identifier_test, [main/0]).

:- use_module(library(os)).
:- use_module('../../../assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module(did_identifier).

test(Spec) :-
    Spec = 'did_identifier:has_only_allowed_chars:failing',
    assert(
        Spec,
        catch(did_identifier:has_only_allowed_chars("_!%1-test.0fr"), type_error(E), true),
        'Subject must contain only allowed characters.',
        E
    ).

test(Spec) :-
    Spec = 'did_identifier:has_only_allowed_chars:succeeding',
    assert(
        Spec,
        (did_identifier:has_only_allowed_chars("_%1-test.0fr"), E = true),
        has_only_allowed_chars:true,
        has_only_allowed_chars:E
    ).

test(Spec) :-
    Spec = 'did_identifier:must_start_with:failing',
    assert(
        Spec,
        (did_identifier:must_start_with("did:", "did:"), StartsWithDidColon = true),
        starts_with_did_colon:true,
        starts_with_did_colon:StartsWithDidColon
    ).

test(Spec) :-
    Spec = 'did_identifier:must_start_with:succeeding',
    assert(
        Spec,
        catch(did_identifier:must_start_with("_did:stuff", "did:"), E, true),
        must_start_with:error_must_start_with("did:"),
        must_start_with:E
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_method_format:failing:1',
    assert(
        Spec,
        catch(
            did_identifier:must_have_valid_method_format("did:A"),
            E,
            true
        ),
        error_not_enough_segments,
        E
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_method_format:failing:2',
    assert(
        Spec,
        catch(
            did_identifier:must_have_valid_method_format("did:A:"),
            E,
            true
        ),
        error_must_not_be_empty,
        E
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_method_format:failing:3',
    assert(
        Spec,
        catch(
            did_identifier:must_have_valid_method_format("did:A:id"),
            E,
            true
        ),
        error_method_must_be_lowercase_alpha_only,
        E
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_method_format:succeeding',
    assert(
        Spec,
        (did_identifier:must_have_valid_method_format("did:a:id"),
        ValidMethodFormat = true),
        valid_method_format:true,
        valid_method_format:ValidMethodFormat
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_identifier_format:failing:1',
    assert(
        Spec,
        catch(
            did_identifier:must_have_valid_identifier_format("did:a:id:"),
            E,
            true
        ),
        error_too_many_segments,
        E
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_identifier_format:failing:2',
    assert(
        Spec,
        catch(
            did_identifier:must_have_valid_identifier_format("did:a:id%"),
            E,
            true
        ),
        error_identifier_must_not_end_with('%'),
        E
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_identifier_format:failing:3',
    assert(
        Spec,
        catch(
            did_identifier:must_have_valid_identifier_format("did:a:id:rest"),
            E,
            true
        ),
        error_too_many_segments,
        E
    ).

test(Spec) :-
    Spec = 'did_identifier:is_valid_did_identifier:failing:1',
    assert(
        Spec,
        catch(
            did_identifier:is_valid_did_identifier("did:weba:i%3A_d-0"),
            E,
            true
        ),
        error_method_must_be_supported,
        E
    ).

test(Spec) :-
    Spec = 'did_identifier:is_valid_did_identifier:succeeding:1',
    assert(
        Spec,
        (did_identifier:is_valid_did_identifier("did:web:i_d-0"),
        IsValidDidIdentifier = true),
        is_valid_did_identifier:true,
        is_valid_did_identifier:IsValidDidIdentifier
    ).

test(Spec) :-
    Spec = 'did_identifier:is_valid_did_identifier:succeeding:2',
    assert(
        Spec,
        (did_identifier:is_valid_did_identifier("did:plc:i%3A_d-0"),
        IsValidDidIdentifier = true),
        is_valid_did_identifier:true,
        is_valid_did_identifier:IsValidDidIdentifier
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_percent_encoding:failing:1',
    assert(
        Spec,
        catch(
            did_identifier:must_have_valid_percent_encoding("i%%"),
            E,
            true
        ),
        error_must_have_at_least_n_characters(2),
        E
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_percent_encoding:failing:2',
    assert(
        Spec,
        catch(
            did_identifier:must_have_valid_percent_encoding("i%1"),
            E,
            true
        ),
        error_must_have_at_least_n_characters(2),
        E
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_percent_encoding:failing:3',
    assert(
        Spec,
        catch(
            did_identifier:must_have_valid_percent_encoding("i%1A"),
            E,
            true
        ),
        error_invalid_percent_encoding,
        E
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_percent_encoding:succeeding:1',
    assert(
        Spec,
        (did_identifier:must_have_valid_percent_encoding("i3A"),
        IsValidPercentEncoding = true),
        is_valid_percent_encoding:true,
        is_valid_percent_encoding:IsValidPercentEncoding
    ).

test(Spec) :-
    Spec = 'did_identifier:must_have_valid_percent_encoding:succeeding:2',
    assert(
        Spec,
        (did_identifier:must_have_valid_percent_encoding("i%3A"),
        IsValidPercentEncoding = true),
        is_valid_percent_encoding:true,
        is_valid_percent_encoding:IsValidPercentEncoding
    ).

main :-
    setenv("LOG_LEVEL", "info"),
    Specs = [
        'did_identifier:has_only_allowed_chars:failing',
        'did_identifier:has_only_allowed_chars:succeeding',
        'did_identifier:must_start_with:failing',
        'did_identifier:must_start_with:succeeding',
        'did_identifier:must_have_valid_method_format:failing:1',
        'did_identifier:must_have_valid_method_format:failing:2',
        'did_identifier:must_have_valid_method_format:failing:3',
        'did_identifier:must_have_valid_method_format:succeeding',
        'did_identifier:must_have_valid_identifier_format:failing:1',
        'did_identifier:must_have_valid_identifier_format:failing:2',
        'did_identifier:must_have_valid_identifier_format:failing:3',
        'did_identifier:is_valid_did_identifier:failing:1',
        'did_identifier:is_valid_did_identifier:succeeding:1',
        'did_identifier:is_valid_did_identifier:succeeding:2',
        'did_identifier:must_have_valid_percent_encoding:failing:1',
        'did_identifier:must_have_valid_percent_encoding:failing:2',
        'did_identifier:must_have_valid_percent_encoding:failing:3',
        'did_identifier:must_have_valid_percent_encoding:succeeding:1',
        'did_identifier:must_have_valid_percent_encoding:succeeding:2'
    ],
    run_test_suite(did_identifier_test, Specs).

:- initialization(main).