:- module(list_member_feed_test, [main/0]).

:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../src/assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module('../src/app/bsky/graph/getList', [
    import_member_feed/2,
    member_feed/2,
    member_feeds/1
]).

/*
Importing a list also fetches each member's recent posts. Recording the
membership must not depend on that fetch: when a page fails (a refused
or rate-limited request, an error while storing a post), member_feed/2
reports a failure or an error instead of propagating it.
*/

/*
The hourly fetch already collects every member's posts, so the import
only fetches them when LIST_IMPORT_MEMBER_FEEDS=1: by default
import_member_feed/2 must not call the fetcher at all.
*/

:- dynamic(fetched_feed/1).

recording_feed(Actor, fetched) :- assertz(fetched_feed(Actor)).

%% feed_fetches(+Setting, -Count)
%
% Runs import_member_feed/2 with LIST_IMPORT_MEMBER_FEEDS set to
% Setting (unset for `unset`) and counts the fetches.
feed_fetches(Setting, Count) :-
    retractall(fetched_feed(_)),
    (   Setting = unset
    ->  unsetenv("LIST_IMPORT_MEMBER_FEEDS")
    ;   setenv("LIST_IMPORT_MEMBER_FEEDS", Setting)
    ),
    import_member_feed(list_member_feed_test:recording_feed, "did:plc:probe"),
    findall(A, fetched_feed(A), As),
    length(As, Count).

option(Setting, Option) :-
    (   Setting = unset
    ->  unsetenv("LIST_IMPORT_MEMBER_FEEDS")
    ;   setenv("LIST_IMPORT_MEMBER_FEEDS", Setting)
    ),
    catch(member_feeds(Option), invalid_list_import_member_feeds(_), Option = refused).

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

test(Spec) :-
    Spec = 'list_member_feed:member_feeds_are_off_by_default',
    option(unset, Option),
    assert(Spec, true, off, Option).
test(Spec) :-
    Spec = 'list_member_feed:an_empty_setting_means_off',
    option("", Option),
    assert(Spec, true, off, Option).
test(Spec) :-
    Spec = 'list_member_feed:zero_means_off',
    option("0", Option),
    assert(Spec, true, off, Option).
test(Spec) :-
    Spec = 'list_member_feed:one_turns_member_feeds_on',
    option("1", Option),
    assert(Spec, true, on, Option).
test(Spec) :-
    Spec = 'list_member_feed:any_other_setting_is_refused',
    option("yes", Option),
    assert(Spec, true, refused, Option).
test(Spec) :-
    Spec = 'list_member_feed:the_import_does_not_fetch_feeds_by_default',
    feed_fetches(unset, Count),
    assert(Spec, true, 0, Count).
test(Spec) :-
    Spec = 'list_member_feed:the_import_fetches_feeds_when_opted_in',
    feed_fetches("1", Count),
    assert(Spec, true, 1, Count).

main :-
    Specs = [
        'list_member_feed:a_failing_fetch_does_not_fail_the_import',
        'list_member_feed:a_throwing_fetch_does_not_abort_the_import',
        'list_member_feed:a_successful_fetch_succeeds',
        'list_member_feed:member_feeds_are_off_by_default',
        'list_member_feed:an_empty_setting_means_off',
        'list_member_feed:zero_means_off',
        'list_member_feed:one_turns_member_feeds_on',
        'list_member_feed:any_other_setting_is_refused',
        'list_member_feed:the_import_does_not_fetch_feeds_by_default',
        'list_member_feed:the_import_fetches_feeds_when_opted_in'
    ],
    run_test_suite(list_member_feed_test, Specs).

:- initialization(main).
