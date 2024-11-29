:- module(at_identifier_test, [
    main/0
]).

:- use_module(at_identifier, [
    has_no_proceeding_ascii_period/1,
    has_no_trailing_ascii_period/1,
    has_only_allowed_chars/1,
    has_only_ascii_chars/1,
    has_two_labels_at_least/1,
    has_valid_length/1,
    is_valid_handle/1,
    not_disallowed_top_level_domain/1,
    not_ending_with_hyphen/1,
    not_starting_with_digit/1,
    not_starting_with_hyphen/1,
    split_subject/3,
    with_labels_having_valid_length/1,
    with_last_label_distinct_from_disallowed_top_level_domain/1
]).
:- use_module(library(freeze)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(when)).

assert(Spec, Goal, Expected, Actual) :-
    (   ( call(Goal), freeze(Actual, Expected = Actual) )
    ->  write('[OK] ')
    ;   write('[KO] '), WriteDiff = true
    ),
    write(Spec), nl,

    (   ground(WriteDiff)
    ->
        call(Goal), !,
        write('expected: '), writeq(Expected), nl,
        write('actual: '), writeq(Actual), nl
    ;   true).

test(Spec) :-
    Spec = 'requires_ground_subject',
    assert(
        Spec,
        catch(split_subject(_Handle, '.', ["jay","bsky"]), instantiation_error(ActualError), true),
        'Subject must be ground.',
        ActualError
    ).

test(Spec) :-
    Spec = 'jay',
    assert(
        Spec,
        split_subject("jay", '.', ActualLabels),
        ["jay"],
        ActualLabels
     ).

test(Spec) :-
    Spec = 'jay.bsky',
    assert(
        Spec,
        split_subject("jay.bsky", '.', ActualLabels),
        ["jay","bsky"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'jay.bsky.social',
    assert(
        Spec,
        split_subject("jay.bsky.social", '.', ActualLabels),
        ["jay","bsky","social"],
        ActualLabels
    ).

test(Spec) :-
    Spec = '8.cn',
    assert(
        Spec,
        split_subject("8.cn", '.', ActualLabels),
        ["8","cn"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'name.t--t',
    assert(
        Spec,
        split_subject("name.t--t", '.', ActualLabels),
        ["name","t--t"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'XX.LCS.MIT.EDU',
    assert(
        Spec,
        split_subject("XX.LCS.MIT.EDU", '.', ActualLabels),
        ["XX","LCS","MIT","EDU"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'a.co',
    assert(
        Spec,
        split_subject("a.co", '.', ActualLabels),
        ["a","co"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'xn--notarealidn.com',
    assert(
        Spec,
        split_subject("xn--notarealidn.com", '.', ActualLabels),
        ["xn--notarealidn","com"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'xn--fiqa61au8b7zsevnm8ak20mc4a87e.xn--fiqs8s',
    assert(
        Spec,
        split_subject("xn--fiqa61au8b7zsevnm8ak20mc4a87e.xn--fiqs8s", '.', ActualLabels),
        ["xn--fiqa61au8b7zsevnm8ak20mc4a87e", "xn--fiqs8s"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'xn--ls8h.test',
    assert(
        Spec,
        split_subject("xn--ls8h.test", '.', ActualLabels),
        ["xn--ls8h", "test"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'example.t',
    assert(
        Spec,
        split_subject("example.t", '.', ActualLabels),
        ["example","t"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'has_at_most_253_characters',
    append([
            "0123401234012340123401234012340123401234012340123401234012340123401234012340123401234012340123401234",
            "0123401234012340123401234012340123401234012340123401234012340123401234012340123401234012340123401234",
            "123401234012340123401234012340123401234012340123401234"
        ],
        SubjectCounting254Chars
    ),

    assert(
        Spec,
        (\+ has_valid_length(SubjectCounting254Chars), AtMost253Characters = false),
        at_most253_characters:false,
        at_most253_characters:AtMost253Characters
    ).

test(Spec) :-
    Spec = 'has_at_most_253_characters',
    append([
            "0123401234012340123401234012340123401234012340123401234012340123401234012340123401234012340123401234",
            "0123401234012340123401234012340123401234012340123401234012340123401234012340123401234012340123401234",
            "12340123401234012340123401234012340123401234012340123"
        ],
        SubjectCounting253Chars
    ),

    assert(
        Spec,
        (has_valid_length(SubjectCounting253Chars), AtMost253Characters = true),
        at_most253_characters:true,
        at_most253_characters:AtMost253Characters
    ).

test(Spec) :-
    Spec = 'has_at_least_two_labels',
    assert(
        Spec,
        (\+ has_two_labels_at_least("jay"), AtLeastTwoLabels = false),
        false,
        AtLeastTwoLabels
    ).

test(Spec) :-
    Spec = 'has_at_least_two_labels',
    assert(
        Spec,
        (has_two_labels_at_least("jay.bsky"), AtLeastTwoLabels = true),
        at_least_two_labels:true,
        at_least_two_labels:AtLeastTwoLabels
    ).

test(Spec) :-
    Spec = 'has_no_proceeding_ascii_period',
    assert(
        Spec,
        catch(
            has_no_proceeding_ascii_period(".jay.bsky"),
            E,
            true
        ),
        error_must_not_start_with('.'),
        E
    ).

test(Spec) :-
    Spec = 'has_no_proceeding_ascii_period',
    assert(
        Spec,
        (has_no_proceeding_ascii_period("jay.bsky"), NoProceedingAsciiPeriod = true),
        no_proceeding_ascii_period:true,
        no_proceeding_ascii_period:NoProceedingAsciiPeriod
    ).

test(Spec) :-
    Spec = 'has_no_trailing_ascii_period',
    assert(
        Spec,
        catch(
            has_no_trailing_ascii_period("jay.bsky."),
            E,
            true
        ),
        error_must_not_end_with('.'),
        E
    ).

test(Spec) :-
    Spec = 'has_no_trailing_ascii_period',
    assert(
        Spec,
        (has_no_trailing_ascii_period("jay.bsky"), NoTrailingAsciiPeriod = true),
        no_trailing_ascii_period:true,
        no_trailing_ascii_period:NoTrailingAsciiPeriod
    ).

test(Spec) :-
    Spec = 'has_only_ascii_chars',
    assert(
        Spec,
        catch(has_only_ascii_chars("😅.test"), type_error(Message), true),
        'Subject must start with an ascii char.',
        Message
    ).

test(Spec) :-
    Spec = 'with_labels_having_valid_length',
    assert(
        Spec,
        catch(
            with_labels_having_valid_length("0123401234012340123401234012340123401234012340123401234012340123.0"),
            E,
            true
        ),
        'invalid_label_length',
        E
    ).

test(Spec) :-
    Spec = 'with_labels_having_valid_length',
    assert(
        Spec,
        (with_labels_having_valid_length("012340123401234012340123401234012340123401234012340123401234012.0"), LabelsHaveValidLength = true),
        true,
        LabelsHaveValidLength
    ).

test(Spec) :-
    Spec = 'has_only_allowed_chars',
    assert(
        Spec,
        catch(has_only_allowed_chars("jo@hn"), type_error(Message), true),
        'Subject must contain only allowed characters.',
        Message
    ).

test(Spec) :-
    Spec = 'not_starting_with_hyphen',
    assert(
        Spec,
        catch(not_starting_with_hyphen("-john"), error_must_not_start_with(C), true),
        '-',
        C
    ).

test(Spec) :-
    Spec = 'not_end_with_hyphen',
    assert(
        Spec,
        catch(has_only_allowed_chars("john-"), error_must_not_end_with(C), true),
        '-',
        C
    ).

test(Spec) :-
    Spec = 'not_starting_with_digit',
    assert(
        Spec,
        catch(not_starting_with_digit("0john"), E, true),
        'error_must_not_start_with_digit',
        E
    ).

test(Spec) :-
    Spec = 'not_starting_with_digit',
    assert(
        Spec,
        (not_starting_with_digit("j0ohn"), NotStartingWithDigit = true),
        not_starting_with_digit:true,
        not_starting_with_digit:NotStartingWithDigit
    ).

test(Spec) :-
    Spec = 'not_disallowed_top_level_domain',
    setenv("ENVIRONMENT", "development"),
    assert(
        Spec,
        (not_disallowed_top_level_domain("test"), NotDisallowedTopLevelDomain = true),
        not_disallowed_top_level_domain:true,
        not_disallowed_top_level_domain:NotDisallowedTopLevelDomain
    ).

test(Spec) :-
    Spec = 'not_disallowed_top_level_domain',
    setenv("ENVIRONMENT", "production"),
    assert(
        Spec,
        catch(not_disallowed_top_level_domain("test"), E, true),
        disallowed_top_level_domain("test"),
        E
    ).

test(Spec) :-
    Spec = 'not_disallowed_top_level_domain',
    setenv("ENVIRONMENT", "testing"),
    assert(
        Spec,
        catch(not_disallowed_top_level_domain("arpa"), E, true),
        disallowed_top_level_domain("arpa"),
        E
    ).

test(Spec) :-
    Spec = 'not_disallowed_top_level_domain',
    setenv("ENVIRONMENT", "testing"),
    assert(
        Spec,
        (not_disallowed_top_level_domain("test"), NotDisallowedTopLevelDomain = true),
        not_disallowed_top_level_domain:true,
        not_disallowed_top_level_domain:NotDisallowedTopLevelDomain
    ).

test(Spec) :-
    Spec = 'with_last_label_distinct_from_disallowed_top_level_domain',
    setenv("ENVIRONMENT", "production"),
    assert(
        Spec,
        catch(with_last_label_distinct_from_disallowed_top_level_domain("joe.onion"), E, true),
        disallowed_top_level_domain("onion"),
        E
    ).

test(Spec) :-
    Spec = 'is_valid_handle',
    assert(
        Spec,
        catch(is_valid_handle("test.-0fr"), E, true),
        is_valid_handle:error_must_not_start_with(-),
        is_valid_handle:E
    ).

test(Spec) :-
    Spec = 'is_valid_handle',
    assert(
        Spec,
        catch(is_valid_handle("test.0-.fr"), E, true),
        is_valid_handle:error_must_not_end_with(-),
        is_valid_handle:E
    ).

test(Spec) :-
    Spec = 'is_valid_handle',
    assert(
        Spec,
        catch(is_valid_handle("test.0fr"), E, true),
        is_valid_handle:error_must_not_start_with_digit,
        is_valid_handle:E
    ).

main :-
    Specs = [
        'requires_ground_subject',
        'jay',
        'jay.bsky',
        'jay.bsky.social',
        '8.cn',
        'name.t--t',
        'XX.LCS.MIT.EDU',
        'a.co',
        'xn--notarealidn.com',
        'xn--fiqa61au8b7zsevnm8ak20mc4a87e.xn--fiqs8s',
        'xn--ls8h.test',
        'example.t',
        'has_strictly_less_than_254_characters',
        'has_at_most_253_characters',
        'has_not_enough_labels',
        'has_at_least_two_labels',
        'has_no_proceeding_ascii_period',
        'has_no_trailing_ascii_period',
        'has_only_ascii_chars',
        'with_labels_having_valid_length',
        'has_only_allowed_chars',
        'not_starting_with_hyphen',
        'not_ending_with_hyphen',
        'not_starting_with_digit',
        'not_disallowed_top_level_domain',
        'with_last_label_distinct_from_disallowed_top_level_domain',
        'is_valid_handle'
    ],
    bagof(member(Spec, Specs), test(Spec), _Sol).

:- initialization(main).