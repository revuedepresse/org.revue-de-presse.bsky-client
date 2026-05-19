:- module(assert, [assert/4, run_test_suite/2]).

/**
Minimal test harness used by the in-tree test suites under `tests/`.

Each test module defines `test/1` clauses, one per spec. Calling
`run_test_suite/2` iterates the given specs, runs each `test/1`
solution under `findall/3`, and classifies the outcome as `ok` if
at least one solution exists. `assert/4` is the single-assertion
primitive that prints `[OK]` / `[KO]` plus an `expected:` /
`actual:` diff on failure.
*/

:- use_module(library(freeze)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(si)).
:- use_module('./types/string/formats/must_be_ground', [must_be_ground/1]).

%% assert(+Spec, :Goal, +Expected, ?Actual)
%
% Print `[OK] Spec` if calling `Goal` succeeds and `Actual`
% eventually unifies (via `freeze/2`) with `Expected`; otherwise
% print `[KO] Spec` followed by an `expected:` / `actual:` diff.
assert(Spec, Goal, Expected, Actual) :-
    must_be_ground(Expected),
    once(write_outcome(Goal, Expected, Actual, WriteDiff)),
    write(Spec), nl,
    maybe_write_diff(WriteDiff, Goal, Expected, Actual).

write_outcome(Goal, Expected, Actual, true) :-
    \+ ( call(Goal), freeze(Actual, \+ dif_si(Expected, Actual) ) ),
    write('[KO] ').
write_outcome(Goal, Expected, Actual, false) :-
    ( call(Goal), freeze(Actual, \+ dif_si(Expected, Actual) ) ),
    write('[OK] ').

maybe_write_diff(true, Goal, Expected, Actual) :-
    \+ \+ ( call(Goal),
            write('expected: '), writeq(Expected), nl,
            write('actual: '), writeq(Actual), nl ).
maybe_write_diff(false, _, _, _).

test_suite(TestModule, Spec, Acc, Result) :-
    findall(s, TestModule:test(Spec), Sols),
    classify_outcome(Sols, Outcome),
    append([Acc, [Outcome]], Result).

classify_outcome([], ko).
classify_outcome([_|_], ok).

%% run_test_suite(+TestModule, +Specs)
%
% Run `TestModule:test(Spec)` for every `Spec` in `Specs` and
% succeed only if every spec produced at least one solution.
% Each test goal is enumerated through `findall/3` so it runs
% exactly once per spec regardless of how many solutions it has.
run_test_suite(TestModule, Specs) :-
    foldl(test_suite(TestModule), Specs, [], Sol),
    forall(member(N, Sol), \+ dif_si(N, ok)).
