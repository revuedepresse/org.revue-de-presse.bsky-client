:- module(split_subject_test, [main/0]).

:- use_module('../../../assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module(split_subject).

test(Spec) :-
    Spec = 'split_subject:requires_ground_subject',
    assert(
        Spec,
        catch(split_subject:split_subject(_Handle, '.', ["jay","bsky"]), instantiation_error(ActualError), true),
        'Subject must be ground.',
        ActualError
    ).

test(Spec) :-
    Spec = 'split_subject:.',
    assert(
        Spec,
        split_subject:split_subject(".", '.', ActualLabels),
        ["",""],
        ActualLabels
     ).

test(Spec) :-
    Spec = 'split_subject:jay',
    assert(
        Spec,
        split_subject:split_subject("jay", '.', ActualLabels),
        ["jay"],
        ActualLabels
     ).

test(Spec) :-
    Spec = 'split_subject:jay.bsky',
    assert(
        Spec,
        split_subject:split_subject("jay.bsky", '.', ActualLabels),
        ["jay","bsky"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'split_subject:jay.bsky.social',
    assert(
        Spec,
        split_subject:split_subject("jay.bsky.social", '.', ActualLabels),
        ["jay","bsky","social"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'split_subject:8.cn',
    assert(
        Spec,
        split_subject:split_subject("8.cn", '.', ActualLabels),
        ["8","cn"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'split_subject:name.t--t',
    assert(
        Spec,
        split_subject:split_subject("name.t--t", '.', ActualLabels),
        ["name","t--t"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'split_subject:XX.LCS.MIT.EDU',
    assert(
        Spec,
        split_subject:split_subject("XX.LCS.MIT.EDU", '.', ActualLabels),
        ["XX","LCS","MIT","EDU"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'split_subject:a.co',
    assert(
        Spec,
        split_subject:split_subject("a.co", '.', ActualLabels),
        ["a","co"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'split_subject:xn--notarealidn.com',
    assert(
        Spec,
        split_subject:split_subject("xn--notarealidn.com", '.', ActualLabels),
        ["xn--notarealidn","com"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'split_subject:xn--fiqa61au8b7zsevnm8ak20mc4a87e.xn--fiqs8s',
    assert(
        Spec,
        split_subject:split_subject("xn--fiqa61au8b7zsevnm8ak20mc4a87e.xn--fiqs8s", '.', ActualLabels),
        ["xn--fiqa61au8b7zsevnm8ak20mc4a87e", "xn--fiqs8s"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'split_subject:xn--ls8h.test',
    assert(
        Spec,
        split_subject:split_subject("xn--ls8h.test", '.', ActualLabels),
        ["xn--ls8h", "test"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'split_subject:example.t',
    assert(
        Spec,
        split_subject:split_subject("example.t", '.', ActualLabels),
        ["example","t"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'split_subject:did:a:id',
    assert(
        Spec,
        (split_subject:split_subject("did:a:id", ':', ActualLabels)),
        ["did","a","id"],
        ActualLabels
    ).

test(Spec) :-
    Spec = 'split_subject:did:a:id:',
    assert(
        Spec,
        (split_subject:split_subject("did:a:id:", ':', ActualLabels)),
        ["did","a","id",""],
        ActualLabels
    ).

main :-
    Specs = [
        'split_subject:requires_ground_subject',
        'split_subject:.',
        'split_subject:jay',
        'split_subject:jay.bsky',
        'split_subject:jay.bsky.social',
        'split_subject:8.cn',
        'split_subject:name.t--t',
        'split_subject:XX.LCS.MIT.EDU',
        'split_subject:a.co',
        'split_subject:xn--notarealidn.com',
        'split_subject:xn--fiqa61au8b7zsevnm8ak20mc4a87e.xn--fiqs8s',
        'split_subject:xn--ls8h.test',
        'split_subject:example.t',
        'split_subject:did:a:id',
        'split_subject:did:a:id:'
    ],
    run_test_suite(split_subject_test, Specs).

:- initialization(main).