:- module(shell_unavailable_test, [main/0]).

:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../src/assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module('../src/os_ext', [
    shell_or_throw/3
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
raising anything, when SHELL is unset or empty (a systemd unit has
none).

Every shell-out goes through os_ext:shell_or_throw/3, which must run
the command under /bin/sh when SHELL is unset or empty, and throw
shell_unavailable/1 when shell/2 still cannot run it (forced here with
a SHELL that does not exist).

The Makefile target starts scryer under `env -u SHELL`; each test sets
up the SHELL it needs itself.
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

%% or_error(+Goal, ?Value)
%
% Runs Goal; a throw or a failure binds Value to threw(E) or failed so
% the assertion reports it instead of aborting the suite.
or_error(Goal, Value) :-
    catch(
        ( call(Goal) -> true ; Value = failed ),
        E,
        Value = threw(E)
    ).

length_or_error(Value, Length) :-
    (   Value = threw(_)
    ->  Length = Value
    ;   Value == failed
    ->  Length = Value
    ;   length(Value, Length)
    ).

without_shell :- unsetenv("SHELL").
empty_shell :- setenv("SHELL", "").
missing_shell :- setenv("SHELL", "/nonexistent/shell").

test(Spec) :-
    Spec = 'shell_unavailable:a_command_runs_without_a_shell',
    without_shell,
    or_error(shell_or_throw("true", 'probe', Status), Status),
    assert(Spec, true, 0, Status).
test(Spec) :-
    Spec = 'shell_unavailable:bin_sh_becomes_the_shell_when_none_is_set',
    without_shell,
    or_error(shell_or_throw("true", 'probe', _), _),
    ( getenv("SHELL", Shell) -> true ; Shell = unset ),
    assert(Spec, true, "/bin/sh", Shell).
test(Spec) :-
    Spec = 'shell_unavailable:a_command_runs_with_an_empty_shell',
    empty_shell,
    or_error(shell_or_throw("true", 'probe', Status), Status),
    assert(Spec, true, 0, Status).
test(Spec) :-
    Spec = 'shell_unavailable:date_iso8601_days_before_computes_without_a_shell',
    without_shell,
    or_error(date_iso8601_days_before(2, "2026-10-24T09:30:00.000Z", Date), Date),
    assert(Spec, true, "2026-10-22", Date).
test(Spec) :-
    Spec = 'shell_unavailable:date_iso8601_days_ago_computes_without_a_shell',
    without_shell,
    or_error(date_iso8601_days_ago(2, Date), Date),
    length_or_error(Date, Length),
    assert(Spec, true, 20, Length).
test(Spec) :-
    Spec = 'shell_unavailable:date_iso8601_computes_without_a_shell',
    without_shell,
    or_error(date_iso8601(Date), Date),
    length_or_error(Date, Length),
    assert(Spec, true, 20, Length).
test(Spec) :-
    Spec = 'shell_unavailable:a_shell_that_cannot_run_throws',
    missing_shell,
    outcome(shell_or_throw("true", 'probe', _), Outcome),
    assert(Spec, true, threw_shell_unavailable, Outcome).
test(Spec) :-
    Spec = 'shell_unavailable:date_iso8601_days_before_throws_when_the_shell_cannot_run',
    missing_shell,
    outcome(date_iso8601_days_before(2, "2026-10-24T09:30:00.000Z", _), Outcome),
    assert(Spec, true, threw_shell_unavailable, Outcome).
test(Spec) :-
    Spec = 'shell_unavailable:beautify_json_throws_when_the_shell_cannot_run',
    missing_shell,
    outcome(beautify_json("/nonexistent-beautify-json-input", _), Outcome),
    assert(Spec, true, threw_shell_unavailable, Outcome).

main :-
    Specs = [
        'shell_unavailable:a_command_runs_without_a_shell',
        'shell_unavailable:bin_sh_becomes_the_shell_when_none_is_set',
        'shell_unavailable:a_command_runs_with_an_empty_shell',
        'shell_unavailable:date_iso8601_days_before_computes_without_a_shell',
        'shell_unavailable:date_iso8601_days_ago_computes_without_a_shell',
        'shell_unavailable:date_iso8601_computes_without_a_shell',
        'shell_unavailable:a_shell_that_cannot_run_throws',
        'shell_unavailable:date_iso8601_days_before_throws_when_the_shell_cannot_run',
        'shell_unavailable:beautify_json_throws_when_the_shell_cannot_run'
    ],
    run_test_suite(shell_unavailable_test, Specs),
    setenv("SHELL", "/bin/sh").

:- initialization(main).
