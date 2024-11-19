:- module(handle_test, [main/0]).

:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(si)).

:- use_module('../../../assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module(handle).

test(Spec) :-
    Spec = 'handle:has_at_most_253_characters:failing',
    append([
            "0123401234012340123401234012340123401234012340123401234012340123401234012340123401234012340123401234",
            "0123401234012340123401234012340123401234012340123401234012340123401234012340123401234012340123401234",
            "123401234012340123401234012340123401234012340123401234"
        ],
        SubjectCounting254Chars
    ),

    assert(
        Spec,
        (\+ (handle:has_valid_length(SubjectCounting254Chars)), AtMost253Characters = false),
        at_most253_characters:false,
        at_most253_characters:AtMost253Characters
    ).

test(Spec) :-
    Spec = 'handle:has_at_most_253_characters:succeeding',
    append([
            "0123401234012340123401234012340123401234012340123401234012340123401234012340123401234012340123401234",
            "0123401234012340123401234012340123401234012340123401234012340123401234012340123401234012340123401234",
            "12340123401234012340123401234012340123401234012340123"
        ],
        SubjectCounting253Chars
    ),

    assert(
        Spec,
        (handle:has_valid_length(SubjectCounting253Chars), AtMost253Characters = true),
        at_most253_characters:true,
        at_most253_characters:AtMost253Characters
    ).

test(Spec) :-
    Spec = 'handle:has_at_least_two_labels:failing',
    assert(
        Spec,
        catch(
            handle:has_two_labels_at_least("jay"),
            E,
            true
        ),
        'error_must_have_two_labels_at_least',
        E
    ).

test(Spec) :-
    Spec = 'handle:has_at_least_two_labels:succeeding',
    assert(
        Spec,
        (handle:has_two_labels_at_least("jay.bsky"), AtLeastTwoLabels = true),
        at_least_two_labels:true,
        at_least_two_labels:AtLeastTwoLabels
    ).

test(Spec) :-
    Spec = 'handle:has_no_proceeding_ascii_period:failing',
    assert(
        Spec,
        catch(
            handle:has_no_proceeding_ascii_period(".jay.bsky"),
            E,
            true
        ),
        error_must_not_start_with('.'),
        E
    ).

test(Spec) :-
    Spec = 'handle:has_no_proceeding_ascii_period:succeeding',
    assert(
        Spec,
        (handle:has_no_proceeding_ascii_period("jay.bsky"), NoProceedingAsciiPeriod = true),
        no_proceeding_ascii_period:true,
        no_proceeding_ascii_period:NoProceedingAsciiPeriod
    ).

test(Spec) :-
    Spec = 'handle:has_no_trailing_ascii_period:failing',
    assert(
        Spec,
        catch(
            handle:has_no_trailing_ascii_period("jay.bsky."),
            E,
            true
        ),
        error_must_not_end_with('.'),
        E
    ).

test(Spec) :-
    Spec = 'handle:has_no_trailing_ascii_period:succeeding',
    assert(
        Spec,
        (handle:has_no_trailing_ascii_period("jay.bsky"), NoTrailingAsciiPeriod = true),
        no_trailing_ascii_period:true,
        no_trailing_ascii_period:NoTrailingAsciiPeriod
    ).

test(Spec) :-
    Spec = 'handle:with_labels_having_valid_length:failing',
    assert(
        Spec,
        catch(
            handle:with_labels_having_valid_length("0123401234012340123401234012340123401234012340123401234012340123.0"),
            E,
            true
        ),
        'invalid_label_length',
        E
    ).

test(Spec) :-
    Spec = 'handle:with_labels_having_valid_length:succeeding',
    assert(
        Spec,
        (handle:with_labels_having_valid_length("012340123401234012340123401234012340123401234012340123401234012.0"), LabelsHaveValidLength = true),
        true,
        LabelsHaveValidLength
    ).

test(Spec) :-
    Spec = 'handle:has_only_allowed_chars:failing',
    assert(
        Spec,
        catch(handle:has_only_allowed_chars("jo@hn"), type_error(Message), true),
        'Subject must contain only allowed characters.',
        Message
    ).

test(Spec) :-
    Spec = 'handle:not_starting_with_hyphen:failing',
    assert(
        Spec,
        catch(handle:not_starting_with_hyphen("-john"), error_must_not_start_with(C), true),
        '-',
        C
    ).

test(Spec) :-
    Spec = 'handle:not_end_with_hyphen:failing',
    assert(
        Spec,
        catch(handle:has_only_allowed_chars("john-"), error_must_not_end_with(C), true),
        '-',
        C
    ).

test(Spec) :-
    Spec = 'handle:not_starting_with_digit:failing',
    assert(
        Spec,
        catch(handle:not_starting_with_digit("0john"), E, true),
        'error_must_not_start_with_digit',
        E
    ).

test(Spec) :-
    Spec = 'handle:not_starting_with_digit:succeeding',
    assert(
        Spec,
        (handle:not_starting_with_digit("j0ohn"), NotStartingWithDigit = true),
        not_starting_with_digit:true,
        not_starting_with_digit:NotStartingWithDigit
    ).

test(Spec) :-
    Spec = 'handle:not_disallowed_top_level_domain:succeeding:1',
    setenv("ENVIRONMENT", "development"),
    assert(
        Spec,
        (handle:not_disallowed_top_level_domain("test"), NotDisallowedTopLevelDomain = true),
        not_disallowed_top_level_domain:true,
        not_disallowed_top_level_domain:NotDisallowedTopLevelDomain
    ).

test(Spec) :-
    Spec = 'handle:not_disallowed_top_level_domain:failing:1',
    setenv("ENVIRONMENT", "production"),
    assert(
        Spec,
        catch(handle:not_disallowed_top_level_domain("test"), E, true),
        disallowed_top_level_domain("test"),
        E
    ).

test(Spec) :-
    Spec = 'handle:not_disallowed_top_level_domain:failing:2',
    setenv("ENVIRONMENT", "testing"),
    assert(
        Spec,
        catch(handle:not_disallowed_top_level_domain("arpa"), E, true),
        disallowed_top_level_domain("arpa"),
        E
    ).

test(Spec) :-
    Spec = 'handle:not_disallowed_top_level_domain:succeeding:2',
    setenv("ENVIRONMENT", "testing"),
    assert(
        Spec,
        (handle:not_disallowed_top_level_domain("test"), NotDisallowedTopLevelDomain = true),
        not_disallowed_top_level_domain:true,
        not_disallowed_top_level_domain:NotDisallowedTopLevelDomain
    ).

test(Spec) :-
    Spec = 'handle:with_last_label_distinct_from_disallowed_top_level_domain:failing',
    setenv("ENVIRONMENT", "production"),
    assert(
        Spec,
        catch(handle:with_last_label_distinct_from_disallowed_top_level_domain("joe.onion"), E, true),
        disallowed_top_level_domain("onion"),
        E
    ).

test(Spec) :-
    Spec = 'handle:is_valid_handle:failing:1',
    assert(
        Spec,
        catch(handle:is_valid_handle("test.-0fr"), E, true),
        is_valid_handle:error_must_not_start_with(-),
        is_valid_handle:E
    ).

test(Spec) :-
    Spec = 'handle:is_valid_handle:failing:2',
    assert(
        Spec,
        catch(handle:is_valid_handle("test.0-.fr"), E, true),
        is_valid_handle:error_must_not_end_with(-),
        is_valid_handle:E
    ).

test(Spec) :-
    Spec = 'handle:is_valid_handle:failing:3',
    assert(
        Spec,
        catch(handle:is_valid_handle("test.0fr"), E, true),
        is_valid_handle:error_must_not_start_with_digit,
        is_valid_handle:E
    ).

test(Spec) :-
    Spec = 'handle:is_valid_handle:succeeding',
    assert(
        Spec,
        (handle:is_valid_handle("test.fr"), IsValidHandle = true),
        is_valid_handle:true,
        is_valid_handle:IsValidHandle
    ).

main :-
    setenv("LOG_LEVEL", "info"),
    Specs = [
        'handle:has_at_most_253_characters:failing',
        'handle:has_at_most_253_characters:succeeding',
        'handle:has_at_least_two_labels:failing',
        'handle:has_at_least_two_labels:succeeding',
        'handle:has_no_proceeding_ascii_period:failing',
        'handle:has_no_proceeding_ascii_period:succeeding',
        'handle:has_no_trailing_ascii_period:failing',
        'handle:has_no_trailing_ascii_period:succeeding',
        'handle:with_labels_having_valid_length:failing',
        'handle:with_labels_having_valid_length:succeeding',
        'handle:has_only_allowed_chars:failing',
        'handle:not_starting_with_hyphen:failing',
        'handle:not_end_with_hyphen:failing',
        'handle:not_starting_with_digit:failing',
        'handle:not_starting_with_digit:succeeding',
        'handle:not_disallowed_top_level_domain:failing:1',
        'handle:not_disallowed_top_level_domain:failing:2',
        'handle:not_disallowed_top_level_domain:succeeding:1',
        'handle:not_disallowed_top_level_domain:succeeding:2',
        'handle:with_last_label_distinct_from_disallowed_top_level_domain:failing',
        'handle:is_valid_handle:failing:1',
        'handle:is_valid_handle:failing:2',
        'handle:is_valid_handle:failing:3',
        'handle:is_valid_handle:succeeding'
    ],
    run_test_suite(handle_test, Specs).

:- initialization(main).