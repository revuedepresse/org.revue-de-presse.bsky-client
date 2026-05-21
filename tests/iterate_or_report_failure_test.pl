:- module(iterate_or_report_failure_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).

:- use_module('../src/app/bsky/feed/getAuthorFeed', [
    report_iteration_failure/1
]).

/*
Regression test for the silent-failure mode behind:

    %:[onGetAuthorFeed_failed_with_posts_count|15]
    Process N dead!

Captures under /tmp/segv-investigation/ end at by_indexed_at:matching_criteria
on the per-page first post. The outer maplist on the feed posts then fails
without throwing, so the second clause of iterate_or_report_failure/5 used
to log the count and silently succeed -- letting send_request/3 carry on
as if the page had processed cleanly.

The sustainable fix relocates the log + throw into a small, exported
predicate (report_iteration_failure/1) so this test can exercise it
directly without needing to construct a failing maplist over a feed.

The throw is caught by the outer catch/3 in
app__bsky__feed__getAuthorFeed_without_memoization/2; the worker now
log_errors the labelled term and fails the request instead of moving on.
*/

run_test :-
    catch(
        getAuthorFeed:report_iteration_failure(15),
        Err,
        Caught = Err
    ),
    (   nonvar(Caught),
        Caught == maplist_silently_failed_over_feed(15)
    ->  format("[OK] getAuthorFeed:report_iteration_failure_throws_after_logging~n", [])
    ;   format("[KO] expected throw maplist_silently_failed_over_feed(15)~n", []),
        format("     got ~q~n", [Caught]),
        halt(1)
    ).

:- initialization(run_test).
