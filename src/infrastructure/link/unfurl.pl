:- module(unfurl, [
    expand_text/3,
    resolve_remote/2
]).

:- use_module(library(http/http_open)).
:- use_module(library(lists)).

:- use_module('../repository/repository_publication_link', [
    attempts_since/3,
    expansion_of/3,
    record_expansion/4,
    record_pending/3,
    record_refusal/4
]).
:- use_module('../../link_expansion', [
    answer_of_outcome/2,
    answer_of_status/2,
    budget_left/3,
    expansion_plan/5,
    outcome_of/4
]).
:- use_module('../../link_shortener', [shortened_urls_in_text/2]).
:- use_module('../../configuration', [
    link_expander_user_agent/1,
    link_expansion/1
]).
:- use_module('../../stream', [
    read_stream/2,
    writeln/2
]).

/**
Replaces the shortened links in a post with the addresses they lead to,
at the moment the post is collected.

This module performs; it decides nothing. What the ledger knows, what is
worth asking about and what to put in the text are a function of data, and
that function is src/link_expansion.pl: a run reads the ledger, hands the
planner the answers it got, performs the questions the plan returns, and
records what came back. The three were once one predicate, and
the rule that should have held a burst back — twelve links an hour, counted
from the ledger — lived inside an effect where nothing could check it. One
run asked some eight hundred times in a row and 812 live links were written
off as refusals.

A link the ledger has never seen is asked for once, by us, under our own
name. `http_open/3` follows the redirects inside the Rust builtin and
never hands back the address it arrived at, so the destination is read
from what the page claims for itself — `<link rel="canonical">`, else
`<meta property="og:url">` — and reduced to a permalink before it is
stored. Reading that reply is itself a relation and lives with the planner;
what is left here is the request.

The redirectors most French publishers use — `l.mediapart.fr`,
`l.humanite.fr`, `l.reporterre.net`, `www.altereco.media` and some twenty
more, all one provider — refuse curl and a headless browser with a `403`
block page, but answer an identified client with a `200` and an
interstitial that bounces through JavaScript. That page carries the
`<noscript>` fallback a client without JavaScript is meant to follow, and
the destination is in it. Services that redirect outright
(`l.franceinfo.fr`, `f.mtr.cool`, `youtu.be`, `bit.ly`, `tinyurl.com`)
are followed by http_open/3 and claim themselves through their canonical
link.

One request per distinct link, ever — and no more than twelve an hour across
the whole deployment, counted from the ledger rather than from this run, so
that two runs cannot each spend the allowance. What there is no budget for is
noted pending: a resolver driving a real browser answers those out of band
and writes back to this same table.

A link the ledger has refused or declared dead is left exactly as the post
wrote it: a reader still gets a working link, and no service is asked twice.
*/

%% expand_text(+Session, +Text, -Expanded)
%
% Threads the session: the ledger is read once per distinct shortened link
% in the post, and written once for each one never seen before.
expand_text(pg_session(C0, C), Text, Expanded) :-
    link_expansion(Setting),
    expand_text_(Setting, pg_session(C0, C), Text, Expanded).

%% expand_text_(+Setting, +Session, +Text, -Expanded)
%
% Switched off, or failing for any reason — a ledger table that is not
% there yet, a query that errors, a redirector that answers something
% unreadable — the post is stored exactly as written. Collecting a post
% is the job; expanding its links is an improvement on it, and an
% improvement must never be the reason a post is lost.
expand_text_("off", pg_session(C, C), Text, Text).
expand_text_("on", pg_session(C0, C), Text, Expanded) :-
    catch(
        expand_known(pg_session(C0, C), Text, Expanded),
        Error,
        ( writeln([link_expansion_unavailable|[Error]], true),
          ensure_bound(pg_session(C0, C)),
          Expanded = Text )
    ).

%% ensure_bound(+Session)
%
% The session's output slot may still be unbound when a DB call threw
% mid-chain; binding it to the input leaves the caller with a usable
% connection and no DB state changed. Two clauses gated on var/1, as
% event_getAuthorFeed.pl does for the same reason.
ensure_bound(pg_session(In, Out)) :-
    var(Out),
    Out = In.
ensure_bound(pg_session(_, Out)) :-
    nonvar(Out).

%% expand_known(+Session, +Text, -Expanded)
%
% The whole shape of the ingest pass: count what the window has already
% spent, read the ledger for the links this post carries, plan, ask, record,
% then plan once more with the answers just learnt beside the ones read.
%
% The second plan is what rewrites the text, and it rewrites the original:
% one pass over it, substitutions in the order the links appear, whether the
% address came from the ledger or from the service a moment ago.
expand_known(pg_session(C0, C), Text, Expanded) :-
    shortened_urls_in_text(Text, Shortened),
    budget(Allowance),
    window_minutes(Minutes),
    attempts_since(pg_session(C0, C1), minutes(Minutes), Spent),
    budget_left(Allowance, Spent, Budget),
    known_answers(pg_session(C1, C2), Shortened, Known),
    expansion_plan(Text, Known, Budget, plan(_, ask(Ask), note(Note)), _),
    ask_each(Ask, pg_session(C2, C3), Learnt),
    note_each(Note, pg_session(C3, C)),
    append(Known, Learnt, Answers),
    expansion_plan(Text, Answers, 0, _, Expanded).

%% budget(-Links) and window_minutes(-Minutes)
%
% At most this many links asked about per window, counted across every run
% from the ledger. Twelve an hour is a reader's pace, not a crawler's, and
% it is what a service refusing datacenter addresses will tolerate while the
% out-of-band resolver does the bulk.
budget(12).
window_minutes(60).

%% known_answers(+Session, +Urls, -Known)
%
% What the ledger has to say about each address, in the planner's
% vocabulary. An address it has never seen contributes no pair at all: the
% planner asks about exactly the links nothing is known about, which is why
% a token shared by a hundred posts costs one question.
known_answers(pg_session(C, C), [], []).
known_answers(pg_session(C0, C), [Url|Urls], Known) :-
    expansion_of(pg_session(C0, C1), short_url(Url), Found),
    found_answer(Found, Url, Known, Rest),
    known_answers(pg_session(C1, C), Urls, Rest).

%% found_answer(+Found, +Url, -Known, -Rest)
%
% The ledger answers `present(expanded(Url))`, `present(Status)` with the
% status as the text of its column, or `absent`. A row's status is text on
% the wire and a marker to the planner; the two are told apart here, at the
% boundary where the row is read, and nowhere else.
found_answer(present(Row), Url, [Url-Answer|Rest], Rest) :-
    ledger_answer(Row, Answer).
found_answer(absent, _, Rest, Rest).

% `expanded/1`, a non-empty status, and a column that came back empty are
% three distinct principal functors, so no clause needs a cut or a `dif/2`
% to keep the others out.
ledger_answer(expanded(Expanded), expanded(Expanded)).
ledger_answer([C|Cs], Answer) :-
    answer_of_status([C|Cs], Answer).
ledger_answer([], pending).

%% ask_each(+Urls, +Session, -Learnt)
%
% One request per address the plan asked for, the answer written to the
% ledger and handed back as the planner's, so the text can be rewritten
% from everything known by the time the post is stored.
ask_each([], pg_session(C, C), []).
ask_each([Url|Urls], pg_session(C0, C), [Url-Answer|Learnt]) :-
    resolve_remote(Url, Outcome),
    remember(Outcome, pg_session(C0, C1), Url),
    answer_of_outcome(Outcome, Answer),
    ask_each(Urls, pg_session(C1, C), Learnt).

%% note_each(+Urls, +Session)
%
% The links the window had no budget for. Nobody is disturbed about them: a
% run that meets eight hundred new links notes them for the resolver that
% works from elsewhere and stores the post as written.
note_each([], pg_session(C, C)).
note_each([Url|Urls], pg_session(C0, C)) :-
    record_pending(pg_session(C0, C1), short_url(Url), _),
    note_each(Urls, pg_session(C1, C)).

%% remember(+Outcome, +Session, +Url)
%
% A refusal is final only in that the service answered: whether the answer
% is about the link is refusal_status/2's to say, in the ledger. An
% unavailable answer — a timeout, a name that does not resolve, a page
% claiming no address of its own — says nothing at all, so it stays pending
% for the browser resolver rather than being written off.
remember(resolved(Expanded), pg_session(C0, C), Url) :-
    record_expansion(pg_session(C0, C), short_url(Url), expanded(Expanded), _).
remember(refused(Code), pg_session(C0, C), Url) :-
    record_refusal(pg_session(C0, C), short_url(Url), http_status(Code), _).
remember(unavailable(Reason), pg_session(C0, C), Url) :-
    writeln([link_unresolved|[Url, Reason]], true),
    record_pending(pg_session(C0, C), short_url(Url), _).

%% resolve_remote(+Url, -Outcome)
%
% One GET, under our own name. Any exception — a name that does not
% resolve, a connection refused, a body that ends mid-stream — becomes
% `unavailable`, never a failure of the ingest: a post is worth storing
% whether or not its links could be followed.
resolve_remote(Url, Outcome) :-
    catch(attempt(Url, Outcome), Error, Outcome = unavailable(Error)).

%% attempt(+Url, -Outcome)
%
% `once/1` guards the foreign call alone: http_open/3 can fail outright —
% a socket the runtime declines to open — and a post is worth storing
% whether or not its links could be followed.
attempt(Url, Outcome) :-
    once(( fetch(Url, Outcome)
         ; Outcome = unavailable(no_answer)
         )).

%% fetch(+Url, -Outcome)
%
% The request, and nothing else: what the status code and the body mean is
% outcome_of/4's, where it can be read without a service to ask.
fetch(Url, Outcome) :-
    user_agent(UserAgent),
    http_open(Url, Stream, [
        status_code(Code),
        request_headers([UserAgent])
    ]),
    read_stream(Stream, Body),
    outcome_of(Code, Url, Body, Outcome).

%% user_agent(-Header)
%
% Named, with an address to complain to: a service that would rather not
% be read by a program can say so, and that refusal is recorded rather
% than worked around.
user_agent('user-agent'(UserAgent)) :-
    link_expander_user_agent(UserAgent).
