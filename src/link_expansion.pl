:- module(link_expansion, [
    answer_of_outcome/2,
    answer_of_status/2,
    budget_left/3,
    destination_outcome/3,
    expansion_decisions/4,
    expansion_plan/5,
    outcome_of/4,
    plan_of/2,
    refusal_status/2,
    resolved_or_same/3
]).

:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(reif)).

:- use_module('link_shortener', [
    page_destination/2,
    permalink/2,
    rewrite_urls/3,
    shortened_urls_in_text/2
]).

/**
What to do with the shortened links a post carries, given what the ledger
already knows and how many questions the hour still allows.

Asking a service is an effect and stays in `src/infrastructure/link/`;
deciding what to ask about, and what to put in the text, is a function and
lives here. The two were once one predicate, and the rules that
should have held a run back had nowhere to live where anything could check
them: one run met some eight hundred links it had never seen, asked for all
of them back to back, and the service's `403`s turned 812 live links into
permanent refusals. A rule stated as a relation can be read, tested and
proved; a rule buried beside `http_open/3` can only be run.

Two vocabularies meet here.

What the ledger knows about an address is an *answer*:

  * `expanded(Url)` — where the link leads, the one answer worth
    substituting;
  * `blocked` — a service that would not serve *us*: this address, this
    rate, this client. Nothing to substitute, and nothing to conclude
    about the link either;
  * `pending` — nobody knows yet; a resolver asking from elsewhere will;
  * `dead` — the link itself is gone (`404`, `410`).

What this module hands back about one link is a *decision*:
`substitute(Url, Expanded)`, `leave(Url, Answer)`, `ask(Url)` or
`note(Url)`. The caller dispatches on the tag; it never has to re-derive
why.

Every predicate is a relation between data and data — no session, no
connection, no request — which is what lets a theorem be written against
it. Definite clauses only: conditions are reified and chosen with `if_/3`,
and the clauses that dispatch on a tag are mutually exclusive by principal
functor, so none of them needs a cut or a `dif/2` to keep the others out.
*/

%% expansion_plan(+Text, +Known, +Budget, -Plan, -Expanded)
%
% Known is a list of `Url-Answer` pairs read from the ledger, Budget the
% number of links this run may still ask about, Plan a
% `plan(substitutions(Pairs), ask(Urls), note(Urls))` and Expanded the text
% with the substitutions already applied.
%
% Expanded answers only from Known: it is the text as the ledger can make
% it right now. A caller that performs the questions in `ask/1` plans again
% with the answers it learnt, so that the final text is rewritten in one
% pass, in the order the links appear.
expansion_plan(Text, Known, Budget, Plan, Expanded) :-
    shortened_urls_in_text(Text, Shortened),
    expansion_decisions(Shortened, Known, Budget, Decisions),
    plan_of(Decisions, Plan),
    Plan = plan(substitutions(Pairs), ask(_), note(_)),
    rewrite_urls(Text, Pairs, Expanded).

%% expansion_decisions(+Urls, +Known, +Budget, -Decisions)
%
% One decision per address, in the order given, the budget running down as
% it goes: the links a run meets first are the ones it asks about, and the
% rest are left for the next hour rather than bursting through.
expansion_decisions([], _, _, []).
expansion_decisions([Url|Urls], Known, Budget, [Decision|Decisions]) :-
    known_answer(Url, Known, Result),
    decision(Result, Url, Budget, Decision, Left),
    expansion_decisions(Urls, Known, Left, Decisions).

%% known_answer(+Url, +Known, -Result)
%
% `answer(Answer)` for the first pair naming that address, else `unknown`.
% First wins, so a caller may append what it has just learnt to what it
% read without pruning either list.
known_answer(_, [], unknown).
known_answer(Url, [Known-Answer|Pairs], Result) :-
    if_(Url = Known,
        Result = answer(Answer),
        known_answer(Url, Pairs, Result)).

%% decision(+Result, +Url, +Budget, -Decision, -Left)
%
% A known destination costs nothing and is substituted. A link the ledger
% has already answered otherwise is left exactly as the post wrote it —
% a reader still gets a working link, and no service is asked twice. Only
% an address nobody has answered spends budget.
decision(answer(Answer), Url, Budget, Decision, Budget) :-
    known_decision(Answer, Url, Decision).
decision(unknown, Url, Budget, Decision, Left) :-
    spend(Budget, Url, Decision, Left).

%% known_decision(+Answer, +Url, -Decision)
%
% `expanded/1` is the only answer that puts an address in the text; every
% other one, named in this module's vocabulary or not, leaves the link as
% the post wrote it and says in the decision which answer it was.
%
% The answer is told by its principal functor rather than by a clause per
% marker, so that this relation is total: an answer nobody foresaw leaves a
% link short, which is a link a reader can still follow, where a failure
% here would travel back through unfurl.pl and drop the whole post. The
% comparison is between two ground terms, which is what keeps `if_/3` from
% leaving a second answer behind it — reifying `Answer = expanded(_)`
% directly would, and that residual choice point is how the old code could
% be made to not substitute a link it had just resolved.
known_decision(Answer, Url, Decision) :-
    functor(Answer, Name, Arity),
    if_(Name/Arity = expanded/1,
        substitution(Answer, Url, Decision),
        Decision = leave(Url, Answer)).

substitution(expanded(Expanded), Url, substitute(Url, Expanded)).

%% spend(+Budget, +Url, -Decision, -Left)
%
% With nothing left the link is noted and nobody is disturbed; otherwise it
% is asked about and the allowance drops by one. A budget below zero — a
% window that overspent — is no budget at all.
%
% The comparison goes through `zcompare/3` and a clause per order, as
% link_shortener.pl does for character ranges. That is not only the style:
% in this Scryer build a clpz comparison written as a clause body goal
% (`spend(B, Url, note(Url), 0) :- B #=< 0.`) is miscompiled and succeeds
% for `B = 9`, which here would mean a run that asks about nothing, or
% about everything.
spend(Budget, Url, Decision, Left) :-
    zcompare(Order, Budget, 0),
    spend_(Order, Budget, Url, Decision, Left).

spend_(<, _, Url, note(Url), 0).
spend_(=, _, Url, note(Url), 0).
spend_(>, Budget, Url, ask(Url), Left) :-
    Left #= Budget - 1.

%% plan_of(+Decisions, -Plan)
%
% Sorts the decisions into the three lists a caller acts on, each in the
% order the links appear in the text: the substitutions to make, the
% addresses to ask about, the addresses to note for a resolver working from
% elsewhere.
plan_of([], plan(substitutions([]), ask([]), note([]))).
plan_of([Decision|Decisions], Plan) :-
    plan_of(Decisions, Rest),
    plan_with(Decision, Rest, Plan).

plan_with(substitute(Url, Expanded),
          plan(substitutions(Pairs), Ask, Note),
          plan(substitutions([Url-Expanded|Pairs]), Ask, Note)).
plan_with(leave(_, _), Plan, Plan).
plan_with(ask(Url),
          plan(Pairs, ask(Ask), Note),
          plan(Pairs, ask([Url|Ask]), Note)).
plan_with(note(Url),
          plan(Pairs, Ask, note(Note)),
          plan(Pairs, Ask, note([Url|Note]))).

%% budget_left(+Allowance, +Spent, -Left)
%
% What an allowance has left once a window's attempts are counted off it.
% A window that overspent — two runs overlapping, a resolver sharing the
% ledger — leaves zero rather than a negative allowance.
budget_left(Allowance, Spent, Left) :-
    zcompare(Order, Allowance, Spent),
    left_of(Order, Allowance, Spent, Left).

left_of(<, _, _, 0).
left_of(=, _, _, 0).
left_of(>, Allowance, Spent, Left) :-
    Left #= Allowance - Spent.

%% outcome_of(+Code, +Url, +Body, -Outcome)
%
% What one answer from a service means: `resolved(Url)`, `refused(Code)` or
% `unavailable(Reason)`. This is the reading of a reply, not the making of
% the request, and it is the piece that was worth the most to get out of the
% effect: whether a status code is a verdict on a link is a statement, and a
% statement can be contradicted by a test.
outcome_of(Code, Url, Body, Outcome) :-
    if_(Code = 200,
        ( page_destination(Body, Found),
          destination_outcome(Found, Url, Outcome) ),
        Outcome = refused(Code)).

%% destination_outcome(+Found, +Url, -Outcome)
%
% A page that claims no address of its own, or claims the one just asked
% for, has not resolved anything.
destination_outcome(none, _, unavailable(no_destination_in_page)).
destination_outcome(destination(Claimed), Url, Outcome) :-
    permalink(Claimed, Expanded),
    resolved_or_same(Expanded, Url, Outcome).

resolved_or_same(Expanded, Url, Outcome) :-
    if_(Expanded = Url,
        Outcome = unavailable(no_destination_in_page),
        Outcome = resolved(Expanded)).

%% refusal_status(+Code, -Status)
%
% Only the two answers that are about the link itself are final. A `403` or
% a `429` is about the asker — this address, this rate, this client — and
% one run's burst turned 812 live links into permanent refusals
% that way. Those are `blocked`: left for a resolver asking from somewhere
% else, and never counted as a verdict on the link.
refusal_status(Code, Status) :-
    if_(memberd_t(Code, [404, 410]),
        Status = "dead",
        Status = "blocked").

%% answer_of_outcome(+Outcome, -Answer)
%
% The same reading again, in the vocabulary the ledger and the planner
% share, so that a caller which has just asked can plan with what it learnt
% beside what it read. A refusal that is not about the link, and an answer
% that never came, both leave the address pending for the resolver asking
% from elsewhere — neither is a reason to stop reading the link to a reader.
answer_of_outcome(resolved(Expanded), expanded(Expanded)).
answer_of_outcome(refused(Code), Answer) :-
    refusal_status(Code, Status),
    answer_of_status(Status, Answer).
answer_of_outcome(unavailable(_), pending).

%% answer_of_status(+Status, -Answer)
%
% The ledger stores a status as the text in its column; the planner reads a
% marker. The two forms are kept apart on purpose: one is what a row says,
% the other is what a decision is made on. A status this module does not
% know is treated as `pending` — known, nothing to substitute, nobody asked
% again — which is what any unexpected row should cost.
answer_of_status(Status, Answer) :-
    known_statuses(Statuses),
    answer_of_status_(Statuses, Status, Answer).

known_statuses(["dead"-dead, "blocked"-blocked, "pending"-pending]).

answer_of_status_([], _, pending).
answer_of_status_([Status-Answer|Pairs], Wanted, Found) :-
    if_(Status = Wanted,
        Found = Answer,
        answer_of_status_(Pairs, Wanted, Found)).
