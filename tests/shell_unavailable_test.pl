:- module(shell_unavailable_test, [main/0]).

:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../src/assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module('../src/temporal', [
    date_iso8601/1,
    date_iso8601_days_ago/2,
    date_iso8601_days_before/3
]).
:- use_module('../src/serialization', [
    beautify_json/2
]).

/*
Scryer's shell/2 runs commands through $SHELL and fails, without
raising anything, when SHELL is unset (a systemd unit has none). A
shell command that cannot run must throw shell_unavailable/1 so the
error is logged.

The Makefile target starts scryer under `env -u SHELL`; the tests also
unset it themselves and set it back for the date checks.
*/

%% outcome(+Goal, -Outcome)
outcome(Goal, Outcome) :-
    catch(
        ( call(Goal) -> Outcome = succeeded ; Outcome = failed ),
        E,
        classify(E, Outcome)
    ).

classify(E, Outcome) :-
    (   E = shell_unavailable(_)
    ->  Outcome = threw_shell_unavailable
    ;   Outcome = threw(E)
    ).

without_shell :- unsetenv("SHELL").
with_shell :- setenv("SHELL", "/bin/sh").

test(Spec) :-
    Spec = 'shell_unavailable:date_iso8601_days_before_throws_without_a_shell',
    without_shell,
    outcome(date_iso8601_days_before(2, "2026-10-24T09:30:00.000Z", _), Outcome),
    assert(Spec, true, threw_shell_unavailable, Outcome).
test(Spec) :-
    Spec = 'shell_unavailable:date_iso8601_days_ago_throws_without_a_shell',
    without_shell,
    outcome(date_iso8601_days_ago(2, _), Outcome),
    assert(Spec, true, threw_shell_unavailable, Outcome).
test(Spec) :-
    Spec = 'shell_unavailable:date_iso8601_throws_without_a_shell',
    without_shell,
    outcome(date_iso8601(_), Outcome),
    assert(Spec, true, threw_shell_unavailable, Outcome).
test(Spec) :-
    Spec = 'shell_unavailable:beautify_json_throws_without_a_shell',
    without_shell,
    outcome(beautify_json("/nonexistent-beautify-json-input", _), Outcome),
    assert(Spec, true, threw_shell_unavailable, Outcome).
test(Spec) :-
    Spec = 'shell_unavailable:date_iso8601_days_before_computes_with_a_shell',
    with_shell,
    date_iso8601_days_before(2, "2026-10-24T09:30:00.000Z", Date),
    assert(Spec, true, "2026-10-22", Date).
test(Spec) :-
    Spec = 'shell_unavailable:date_iso8601_days_ago_computes_with_a_shell',
    with_shell,
    date_iso8601_days_ago(2, Date),
    length(Date, Length),
    assert(Spec, true, 20, Length).

main :-
    Specs = [
        'shell_unavailable:date_iso8601_days_before_throws_without_a_shell',
        'shell_unavailable:date_iso8601_days_ago_throws_without_a_shell',
        'shell_unavailable:date_iso8601_throws_without_a_shell',
        'shell_unavailable:beautify_json_throws_without_a_shell',
        'shell_unavailable:date_iso8601_days_before_computes_with_a_shell',
        'shell_unavailable:date_iso8601_days_ago_computes_with_a_shell'
    ],
    run_test_suite(shell_unavailable_test, Specs).

:- initialization(main).
