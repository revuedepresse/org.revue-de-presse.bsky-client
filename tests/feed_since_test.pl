:- module(feed_since_test, [main/0]).

:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../src/assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module('../src/app/bsky/feed/getAuthorFeed', [
    feed_since/1,
    feed_start/2,
    resolve_cutoff_date/3
]).

/*
getAuthorFeed pages back until its cut-off date: two days before the
newest post by default. FEED_SINCE=YYYY-MM-DD moves the cut-off to
that date, to backfill an account's posts once; without it nothing
changes.

FEED_CURSOR=<cursor> starts that backfill at the page a previous run
stopped at, instead of fetching again the pages it already stored.
*/

% Outcomes are computed here: assert/4 runs its goal in its own module,
% where the predicates under test are not visible.
test(Spec) :-
    Spec = 'feed_since:default_cutoff_is_two_days_before_the_anchor',
    resolve_cutoff_date(no_since, "2026-10-24T09:30:00.000Z", Cutoff),
    assert(Spec, true, "2026-10-22", Cutoff).
test(Spec) :-
    Spec = 'feed_since:a_since_date_is_the_cutoff',
    resolve_cutoff_date(since("2026-05-25"), "2026-10-24T09:30:00.000Z", Cutoff),
    assert(Spec, true, "2026-05-25", Cutoff).
test(Spec) :-
    Spec = 'feed_since:a_since_date_applies_without_an_anchor',
    resolve_cutoff_date(since("2026-05-25"), none, Cutoff),
    assert(Spec, true, "2026-05-25", Cutoff).
test(Spec) :-
    Spec = 'feed_since:unset_means_no_since',
    unsetenv("FEED_SINCE"), feed_since(Since),
    assert(Spec, true, no_since, Since).
test(Spec) :-
    Spec = 'feed_since:read_from_the_environment',
    setenv("FEED_SINCE", "2026-05-25"), feed_since(Since),
    assert(Spec, true, since("2026-05-25"), Since).
test(Spec) :-
    Spec = 'feed_since:an_empty_value_means_no_since',
    setenv("FEED_SINCE", ""), feed_since(Since),
    assert(Spec, true, no_since, Since).
test(Spec) :-
    Spec = 'feed_since:a_malformed_date_is_refused',
    setenv("FEED_SINCE", "25/05/2026"),
    catch(( feed_since(_), Outcome = accepted ), invalid_feed_since(_), Outcome = refused),
    assert(Spec, true, refused, Outcome).

test(Spec) :-
    Spec = 'feed_start:unset_starts_at_the_newest_page',
    unsetenv("FEED_CURSOR"), feed_start("probe.bsky.social", Params),
    assert(Spec, true, "probe.bsky.social", Params).
test(Spec) :-
    Spec = 'feed_start:an_empty_cursor_starts_at_the_newest_page',
    setenv("FEED_CURSOR", ""), feed_start("probe.bsky.social", Params),
    assert(Spec, true, "probe.bsky.social", Params).
test(Spec) :-
    Spec = 'feed_start:a_cursor_starts_at_its_page',
    setenv("FEED_CURSOR", "2025-09-30T17:16:58.734Z"), feed_start("probe.bsky.social", Params),
    unsetenv("FEED_CURSOR"),
    assert(Spec, true, actor("probe.bsky.social")-cursor("2025-09-30T17:16:58.734Z"), Params).
test(Spec) :-
    Spec = 'feed_start:a_malformed_cursor_is_refused',
    setenv("FEED_CURSOR", "yesterday"),
    catch(( feed_start("probe.bsky.social", _), Outcome = accepted ), invalid_feed_cursor(_), Outcome = refused),
    unsetenv("FEED_CURSOR"),
    assert(Spec, true, refused, Outcome).

test(Spec) :-
    Spec = 'feed_start:a_date_without_its_time_is_refused',
    setenv("FEED_CURSOR", "2025-09-30"),
    catch(( feed_start("probe.bsky.social", _), Outcome = accepted ), invalid_feed_cursor(_), Outcome = refused),
    unsetenv("FEED_CURSOR"),
    assert(Spec, true, refused, Outcome).

main :-
    Specs = [
        'feed_since:default_cutoff_is_two_days_before_the_anchor',
        'feed_since:a_since_date_is_the_cutoff',
        'feed_since:a_since_date_applies_without_an_anchor',
        'feed_since:unset_means_no_since',
        'feed_since:read_from_the_environment',
        'feed_since:an_empty_value_means_no_since',
        'feed_since:a_malformed_date_is_refused',
        'feed_start:unset_starts_at_the_newest_page',
        'feed_start:an_empty_cursor_starts_at_the_newest_page',
        'feed_start:a_cursor_starts_at_its_page',
        'feed_start:a_malformed_cursor_is_refused',
        'feed_start:a_date_without_its_time_is_refused'
    ],
    run_test_suite(feed_since_test, Specs).

:- initialization(main).
