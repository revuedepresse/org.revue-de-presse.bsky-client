:- module(expand_day, [
    expand_day_links/1,
    expand_day_links/2
]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(time)).

:- use_module('unfurl', [resolve_remote/2]).
:- use_module('../pg/connection', [
    close_pg_connection/1,
    open_pg_connection/1
]).
:- use_module('../repository/repository_publication_link', [
    links_of_day/3,
    record_expansion/4,
    record_pending/3,
    record_refusal/4,
    rewrite_links_of_day/4
]).
:- use_module('../../configuration', [
    link_expander_pace/1,
    link_expansion_batch/1
]).
:- use_module('../../link_shortener', [shortened_url_t/2]).
:- use_module('../../stream', [writeln/2]).

/**
Expands the shortened links of one day, before that day is ranked.

The collector resolves almost nothing itself: a run meets far more links
than a service will answer in one sitting, and on 2026-11-15 one that
tried was shut out after twenty. This runs once, in the quiet before the
22:00 end-of-day query, and asks slowly:

  * `link_expander_pace/1` seconds between two requests — five by
    default, a reader opening articles rather than a crawler;
  * `link_expansion_batch/1` links at most in a run — sixty by default,
    which is five minutes of asking and more than a day brings;
  * the links come from the rows themselves, so a day can be expanded
    whatever happened while it was being collected;
  * what is learnt goes into the ledger, and the day's stored text is
    rewritten from it, so the ranking written at 22:15 carries the
    addresses the links lead to rather than the tokens.

A service that refuses is recorded `blocked` and left alone — for a
resolver asking from elsewhere, not for the next run of this one.
*/

%% expand_day_links(+Day)
%
% Opens its own connection: this is a task, not a step inside one.
expand_day_links(Day) :-
    open_pg_connection(C0),
    expand_day_links(pg_session(C0, C), Day),
    close_pg_connection(C).

%% expand_day_links(+Session, +Day)
expand_day_links(pg_session(C0, C), Day) :-
    links_of_day(pg_session(C0, C1), day(Day), Urls),
    tfilter(shortened_url_t, Urls, Shortened),
    length(Shortened, Pending),
    link_expansion_batch(Batch),
    take(Batch, Shortened, Asked),
    length(Asked, Count),
    format("expanding ~d of ~d shortened links for ~s~n", [Count, Pending, Day]),
    ask_each(Asked, pg_session(C1, C), Day, 0-0),
    format("done~n", []).

take(0, _, []).
take(N, [], []) :-
    N > 0.
take(N, [X|Xs], [X|Taken]) :-
    N > 0,
    Left is N - 1,
    take(Left, Xs, Taken).

%% ask_each(+Urls, +Session, +Day, +Resolved-Refused)
%
% One request, then a pause, then the next. The pause comes before the
% next request rather than after the last, so a run ends when its work
% does.
ask_each([], pg_session(C, C), _, Resolved-Refused) :-
    format("resolved ~d, left alone ~d~n", [Resolved, Refused]).
ask_each([Url|Urls], pg_session(C0, C), Day, Resolved-Refused) :-
    resolve_remote(Url, Outcome),
    record_and_rewrite(Outcome, pg_session(C0, C1), Day, Url),
    tally(Outcome, Resolved-Refused, Tally),
    pause_between(Urls),
    ask_each(Urls, pg_session(C1, C), Day, Tally).

pause_between([]).
pause_between([_|_]) :-
    link_expander_pace(Seconds),
    sleep(Seconds).

tally(resolved(_), Resolved-Refused, Next-Refused) :-
    Next is Resolved + 1.
tally(refused(_), Resolved-Refused, Resolved-Next) :-
    Next is Refused + 1.
tally(unavailable(_), Resolved-Refused, Resolved-Next) :-
    Next is Refused + 1.

%% record_and_rewrite(+Outcome, +Session, +Day, +Url)
%
% A resolved link is written into the ledger and into the day's rows at
% once: a run interrupted halfway leaves the day half expanded rather
% than a ledger that knows what the rows do not.
record_and_rewrite(resolved(Expanded), pg_session(C0, C), Day, Url) :-
    record_expansion(pg_session(C0, C1), short_url(Url), expanded(Expanded), _),
    rewrite_links_of_day(pg_session(C1, C), day(Day), short_url(Url)-expanded(Expanded), _).
record_and_rewrite(refused(Code), pg_session(C0, C), _, Url) :-
    record_refusal(pg_session(C0, C), short_url(Url), http_status(Code), _).
record_and_rewrite(unavailable(Reason), pg_session(C0, C), _, Url) :-
    writeln([link_unresolved|[Url, Reason]], true),
    record_pending(pg_session(C0, C), short_url(Url), _).
