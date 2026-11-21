/* LPTP mirror of the expansion planner, src/link_expansion.pl.

   This file is PLAIN Prolog with no directives, because LPTP's gnd__compile/1
   reads it and derives the completion and induction axioms the proofs in
   planner.pr rest on. It is not loaded by the worker and never will be: LPTP
   checks a fragment of Prolog that the shipped module does not live in
   (if_/3 is a library call it cannot see through, clpz arithmetic is a solver
   it has no axioms for), so a theorem has to be stated about a transcription.

   The transcription is the weak point of the whole arrangement and is named
   as such in proofs/obligations.tsv (row P4): nothing mechanical says this
   file still agrees with src/link_expansion.pl, so a change there that is not
   made here leaves the proofs standing over code that no longer exists.
   Each encoding below is listed so that agreement can at least be read off by
   eye, and so that it is clear which theorems transfer and which do not.

   ENCODINGS

   1. `if_(C, Then, Else)` is unfolded into a reified call plus one clause per
      truth value -- which is what if_/3 means, and what link_shortener.pl
      already writes by hand for character ranges. k_eq/3 is reif's =/3; its
      false clause is negation as failure on a ground goal, which is where
      LPTP's `fails` reading is exact. Every address, status code and status
      text the planner compares is ground when it is compared, so that
      condition holds throughout.

   2. The budget is a Peano numeral: 0, s(0), s(s(0)), ... in place of a clpz
      integer. The planner's own guard is what makes this faithful rather than
      convenient: `spend/4` goes through zcompare/3 and treats every budget at
      or below zero as spent, and `budget_left/3` hands out zero rather than a
      negative allowance, so no negative number ever reaches a decision. The
      two branches of zcompare that matter (`=<0` and `>0`) are exactly the two
      shapes of a Peano numeral, and `Left #= Budget - 1` on a positive budget
      is exactly the predecessor. What the mirror does NOT carry is the proof
      that clpz's arithmetic agrees with Peano arithmetic, nor the miscompiled
      comparison that the source comment on spend/4 records; a theorem proved
      here is a theorem about the planner's rule, not about Scryer's solver.

   3. The ledger's status column is text in the shipped code ("dead",
      "blocked", "pending") and an atom here (dead, blocked, pending). The map
      is injective and k_eq/3 compares the two forms the same way, so a proof
      about the atoms is a proof about the strings.

   4. An answer's tag is told by functor/3 and a reified Name/Arity comparison
      in the shipped code, which LPTP cannot see through either. k_expanded/2
      replaces it with one clause per answer the module's vocabulary names.
      That is a RESTRICTION, and the only one that narrows a theorem's domain:
      the shipped known_decision/3 is total on every term, deliberately, so
      that an answer nobody foresaw leaves a link short instead of dropping the
      post. The theorems below say nothing about such an answer.

   5. page_destination/2 and permalink/2 belong to the recogniser and are not
      transcribed. They appear here as relations with no constraint at all --
      k_page_destination/2 may answer anything, k_permalink/2 may return
      anything. That OVER-approximates them, which is the safe direction for
      every theorem in planner.pr, because all of them read `succeeds ... =>
      conclusion`: widening what the called relation may answer widens the set
      of runs the theorem quantifies over, so a theorem proved here holds for
      whatever the recogniser actually answers. It would be the unsafe
      direction for an existence or completeness claim, and planner.pr states
      none over these two.

   6. Predicate names carry a k_ prefix, so that this mirror and the shipped
      module can be loaded in one session without either shadowing the other.

   Beyond the mirror, three relations are defined here that have no counterpart
   in src/: k_member/2, k_leq/2 and k_asks/2. They are not transcriptions of
   anything -- they are the vocabulary the theorems are stated in (membership
   of the ledger list, the order on budgets, the number of questions a
   decision list carries). They are defined where the proofs can induct over
   them rather than pulled from LPTP's own list and nat libraries, so that this
   mirror is the single file a reader has to check against src/. */

/* ── the vocabulary the theorems are stated in ───────────────────────────── */

k_member(X, [X|_L]).
k_member(X, [_Y|L]) :- k_member(X, L).

k_leq(0, _Y).
k_leq(s(X), s(Y)) :- k_leq(X, Y).

/* How many questions a decision list carries. One clause per decision tag,
   rather than a counter guarded by a reified test, because the number is what
   the budget theorem is about: dispatching on the tag here is what lets the
   completion axiom for a single cons step name the tag and the count in one
   stroke. */

k_asks([], 0).
k_asks([substitute(_U, _E)|Ds], N) :- k_asks(Ds, N).
k_asks([leave(_U, _A)|Ds], N) :- k_asks(Ds, N).
k_asks([ask(_U)|Ds], s(N)) :- k_asks(Ds, N).
k_asks([note(_U)|Ds], N) :- k_asks(Ds, N).

/* The three decisions that cost nothing. Named as a relation so that the
   budget theorem can case-split two ways instead of four, and so that "this
   decision disturbed no service" is a statement rather than an enumeration
   repeated at every use. */

k_ask_free(substitute(_U, _E)).
k_ask_free(leave(_U, _A)).
k_ask_free(note(_U)).

/* ── reified equality (library(reif)'s =/3, on ground arguments) ──────────── */

k_eq(X, X, true).
k_eq(X, Y, false) :- \+ X = Y.

/* ── the planner (expansion_decisions/4 and what it calls) ────────────────── */

k_decisions([], _Known, _B, []).
k_decisions([U|Us], Known, B, [D|Ds]) :-
    k_known_answer(U, Known, R),
    k_decision(R, U, B, D, Left),
    k_decisions(Us, Known, Left, Ds).

k_known_answer(_U, [], unknown).
k_known_answer(U, [K-A|Ps], R) :-
    k_eq(U, K, B),
    k_known_answer_(B, U, A, Ps, R).

k_known_answer_(true, _U, A, _Ps, answer(A)).
k_known_answer_(false, U, _A, Ps, R) :- k_known_answer(U, Ps, R).

k_decision(answer(A), U, B, D, B) :- k_known_decision(A, U, D).
k_decision(unknown, U, B, D, Left) :- k_spend(B, U, D, Left).

k_known_decision(A, U, D) :-
    k_expanded(A, B),
    k_known_decision_(B, A, U, D).

k_known_decision_(true, expanded(E), U, substitute(U, E)).
k_known_decision_(false, A, U, leave(U, A)).

/* Encoding 4: the reified `Name/Arity = expanded/1` of the shipped code, over
   the four answers the module's vocabulary names. */

k_expanded(expanded(_E), true).
k_expanded(blocked, false).
k_expanded(pending, false).
k_expanded(dead, false).

/* Encoding 2: the two branches of zcompare/3 are the two shapes of a numeral.
   A spent budget notes the link and stays spent; a budget with one question
   left spends it and hands back its predecessor. */

k_spend(0, U, note(U), 0).
k_spend(s(N), U, ask(U), N).

/* ── the plan the caller acts on (plan_of/2) ──────────────────────────────── */

k_plan_of([], plan(substitutions([]), ask([]), note([]))).
k_plan_of([D|Ds], P) :-
    k_plan_of(Ds, Rest),
    k_plan_with(D, Rest, P).

k_plan_with(substitute(U, E),
            plan(substitutions(Ps), Ask, Note),
            plan(substitutions([U-E|Ps]), Ask, Note)).
k_plan_with(leave(_U, _A), P, P).
k_plan_with(ask(U), plan(Ps, ask(As), Note), plan(Ps, ask([U|As]), Note)).
k_plan_with(note(U), plan(Ps, Ask, note(Ns)), plan(Ps, Ask, note([U|Ns]))).

/* ── the allowance (budget_left/3) ────────────────────────────────────────── */

/* A window that overspent leaves nothing rather than a negative allowance,
   which in Peano form is the only thing it could leave. */

k_budget_left(0, _Spent, 0).
k_budget_left(s(A), 0, s(A)).
k_budget_left(s(A), s(S), L) :- k_budget_left(A, S, L).

/* ── reading one answer from a service (outcome_of/4 and what it calls) ───── */

/* Encoding 5: these two are the recogniser's, left unconstrained on purpose. */

k_page_destination(_Body, _Found).
k_permalink(_Claimed, _Expanded).

k_outcome_of(C, U, Body, O) :-
    k_eq(C, 200, B),
    k_outcome_of_(B, C, U, Body, O).

k_outcome_of_(true, _C, U, Body, O) :-
    k_page_destination(Body, F),
    k_destination_outcome(F, U, O).
k_outcome_of_(false, C, _U, _Body, refused(C)).

k_destination_outcome(none, _U, unavailable(no_destination_in_page)).
k_destination_outcome(destination(Claimed), U, O) :-
    k_permalink(Claimed, E),
    k_resolved_or_same(E, U, O).

k_resolved_or_same(E, U, O) :-
    k_eq(E, U, B),
    k_resolved_or_same_(B, E, O).

k_resolved_or_same_(true, _E, unavailable(no_destination_in_page)).
k_resolved_or_same_(false, E, resolved(E)).

/* ── which refusals are verdicts on the link (refusal_status/2) ───────────── */

k_refusal_status(C, S) :-
    k_memberd(C, [404, 410], B),
    k_refusal_status_(B, S).

k_refusal_status_(true, dead).
k_refusal_status_(false, blocked).

/* memberd_t/2 of library(reif), unfolded. */

k_memberd(_X, [], false).
k_memberd(X, [Y|Ys], B) :-
    k_eq(X, Y, E),
    k_memberd_(E, X, Ys, B).

k_memberd_(true, _X, _Ys, true).
k_memberd_(false, X, Ys, B) :- k_memberd(X, Ys, B).

/* ── from an outcome to the answer the planner reads ──────────────────────── */

k_answer_of_outcome(resolved(E), expanded(E)).
k_answer_of_outcome(refused(C), A) :-
    k_refusal_status(C, S),
    k_answer_of_status(S, A).
k_answer_of_outcome(unavailable(_R), pending).

k_answer_of_status(S, A) :-
    k_known_statuses(Ss),
    k_answer_of_status_(Ss, S, A).

k_known_statuses([dead-dead, blocked-blocked, pending-pending]).

k_answer_of_status_([], _W, pending).
k_answer_of_status_([S-A|Ps], W, F) :-
    k_eq(S, W, B),
    k_answer_of_status__(B, A, Ps, W, F).

k_answer_of_status__(true, A, _Ps, _W, A).
k_answer_of_status__(false, _A, Ps, W, F) :- k_answer_of_status_(Ps, W, F).
