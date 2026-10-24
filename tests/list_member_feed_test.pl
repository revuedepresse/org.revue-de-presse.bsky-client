:- module(list_member_feed_test, [main/0]).

:- use_module(library(lists)).

:- use_module('../src/assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module('../src/app/bsky/graph/getList', [
    member_feed/2
]).

/*
Importing a list also fetches each member's recent posts. Recording the
membership must not depend on that fetch: when a page fails (a refused
or rate-limited request, an error while storing a post), member_feed/2
reports a failure or an error instead of propagating it.
*/

failing_feed(_Actor, _Props) :- fail.
throwing_feed(_Actor, _Props) :- throw(failed_http_request(probe, [], 429)).
succeeding_feed(_Actor, fetched).

outcome(Goal, Outcome) :-
    (   catch(member_feed(Goal, "did:plc:probe"), E, (Outcome = threw(E), true))
    ->  ( var(Outcome) -> Outcome = succeeded ; true )
    ;   Outcome = failed
    ).

test(Spec) :-
    Spec = 'list_member_feed:a_failing_fetch_does_not_fail_the_import',
    outcome(list_member_feed_test:failing_feed, Outcome),
    assert(Spec, true, succeeded, Outcome).
test(Spec) :-
    Spec = 'list_member_feed:a_throwing_fetch_does_not_abort_the_import',
    outcome(list_member_feed_test:throwing_feed, Outcome),
    assert(Spec, true, succeeded, Outcome).
test(Spec) :-
    Spec = 'list_member_feed:a_successful_fetch_succeeds',
    outcome(list_member_feed_test:succeeding_feed, Outcome),
    assert(Spec, true, succeeded, Outcome).

main :-
    Specs = [
        'list_member_feed:a_failing_fetch_does_not_fail_the_import',
        'list_member_feed:a_throwing_fetch_does_not_abort_the_import',
        'list_member_feed:a_successful_fetch_succeeds'
    ],
    run_test_suite(list_member_feed_test, Specs).

:- initialization(main).
