:- module(assert, [assert/4, run_test_suite/2]).

:- use_module(library(freeze)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(si)).
:- use_module('./types/string/formats/must_be_ground', [must_be_ground/1]).

assert(Spec, Goal, Expected, Actual) :-
    must_be_ground(Expected),

    (   \+ ( call(Goal), freeze(Actual, \+ dif_si(Expected, Actual) ) )
    ->  write('[KO] '), WriteDiff = true
    ;   write('[OK] ')
    ),
    write(Spec), nl,

    (   ground(WriteDiff)
    ->
        (
            call(Goal),
            write('expected: '), writeq(Expected), nl,
            write('actual: '), writeq(Actual), nl,
            fail
        )
    ;   true ).

test_suite(TestModule,Spec, Acc, Result) :-
    \+  (TestModule:test(Spec))
    ->  append([Acc, [ko]], Result)
    ;   append([Acc, [ok]], Result).

run_test_suite(TestModule, Specs) :-
    foldl(test_suite(TestModule), Specs, [], Sol),
    forall(member(N, Sol), \+ dif_si(N, ok)).