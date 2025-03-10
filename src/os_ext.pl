:- module(os_ext, [
    remove_temporary_file/1,
    temporary_file/2,
    temporary_file/3
]).

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

%% remove_temporary_file(+TempFile).
remove_temporary_file(TempFile) :-
    (   delete_file(TempFile)
    ->  writeln(removed(TempFile))
    ;   throw(unexpected_command_exit_code('Failed to remove temporary file'))).

%% temporary_file(+Prefix, +Suffix, -TempFile).
temporary_file(Prefix, Suffix, TempFile) :-
    temporary_file(Prefix, PrefixedFile),
    append([PrefixedFile, Suffix], TempFile).

%% temporary_file(+Prefix, -TempFile).
temporary_file(Prefix, TempFile) :-
    uuidv4_string(UuidStr),
    temporary_dir(TempDir),
    append([TempDir, "/", Prefix, "_", UuidStr], TempFile).
