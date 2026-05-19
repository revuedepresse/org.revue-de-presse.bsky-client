:- module(assert, [assert/4, run_test_suite/2]).

:- use_module(library(freeze)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(si)).
:- use_module('./types/string/formats/must_be_ground', [must_be_ground/1]).

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

run_test_suite(TestModule, Specs) :-
    foldl(test_suite(TestModule), Specs, [], Sol),
    forall(member(N, Sol), \+ dif_si(N, ok)).