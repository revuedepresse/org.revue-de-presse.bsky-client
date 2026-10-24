:- module(os_ext, [
    remove_temporary_file/1,
    shell_or_throw/3,
    temporary_file/2,
    temporary_file/3
]).

/**
Temporary file helpers.

Allocates uuid-suffixed file paths inside `TEMP_DIR` and
deletes them. Used wherever the worker has to shell out to
`date(1)`, `jq`, or similar to capture command output.
*/

:- use_module(library(charsio)).
:- use_module(library(files)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(uuid)).

:- use_module(configuration, [
    temporary_dir/1
]).
:- use_module(logger, [
    log_debug/1,
    log_info/1
]).
:- use_module(stream, [
    read_stream/2,
    writeln/1,
    writeln/2
]).

%% shell_or_throw(+Command, +Label, -Status)
%
% Run `Command` with shell/2 and unify `Status` with its exit status.
% This is the only shell/2 call in src/ (tests/no_direct_shell_call_test.sh
% checks it). Scryer's shell/2 runs the command through `$SHELL` and
% fails, without raising anything, when SHELL is unset or empty (a
% systemd unit has none), so SHELL is first set to /bin/sh for the
% process in that case, as cron does. If shell/2 still cannot run the
% command (a SHELL that does not exist), it throws
% `shell_unavailable(Label)` so callers' catch and log paths see it.
% `Label` says what the command was for; the command itself is not part
% of the error, as it can hold credentials (the psql client's
% PGPASSWORD).
shell_or_throw(Command, Label, Status) :-
    ensure_shell,
    (   shell(Command, Status)
    ->  true
    ;   throw(shell_unavailable(Label))
    ).

%% ensure_shell
%
% Sets SHELL to /bin/sh when it is unset or empty.
ensure_shell :-
    (   getenv("SHELL", Shell), Shell \== []
    ->  true
    ;   setenv("SHELL", "/bin/sh"),
        log_info([shell_defaulted_to('/bin/sh')])
    ).

%% remove_temporary_file(+TempFile)
%
% Delete `TempFile`. Throws if the deletion fails.
remove_temporary_file(TempFile) :-
    once(remove_temporary_file_or_throw(TempFile)).

remove_temporary_file_or_throw(TempFile) :-
    delete_file(TempFile),
    writeln(removed(TempFile)).
remove_temporary_file_or_throw(TempFile) :-
    \+ delete_file(TempFile),
    throw(unexpected_command_exit_code('Failed to remove temporary file', TempFile)).

%% temporary_file(+Prefix, +Suffix, -TempFile)
%
% Build a temporary file path `<TEMP_DIR>/<Prefix>_<UUID><Suffix>`.
% Does not create the file.
temporary_file(Prefix, Suffix, TempFile) :-
    temporary_file(Prefix, PrefixedFile),
    append([PrefixedFile, Suffix], TempFile).

%% temporary_file(+Prefix, -TempFile)
%
% Build a temporary file path `<TEMP_DIR>/<Prefix>_<UUID>`. Does
% not create the file.
temporary_file(Prefix, TempFile) :-
    uuidv4_string(UuidStr),
    temporary_dir(TempDir),
    append([TempDir, "/", Prefix, "_", UuidStr], TempFile).
