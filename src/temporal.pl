:- module(temporal, [
    date_iso8601/1
]).

:- use_module(library(os)).
:- use_module(library(lists)).
:- use_module(library(uuid)).

:- use_module(configuration, [temporary_dir/1]).
:- use_module(logger, [
    log_debug/1,
    log_info/1
]).
:- use_module(stream, [read_stream/3]).

% temporary_file(+Prefix, -TempFile).
temporary_file(Prefix, TempFile) :-
    uuidv4_string(UuidStr),
    temporary_dir(TempDir),
    append([TempDir, "/", Prefix, "_", UuidStr], TempFile).

% See [date command --iso-8601 option](https://unix.stackexchange.com/a/629504)
% date_iso8601(-Date).
date_iso8601(Iso8601Date) :-
    temporary_file("date-iso-8601", TempFile),

    append(["touch ", TempFile, "; echo -n $(date -u +%Y-%m-%dT%H:%M:%SZ) > ", TempFile], GetDateCmd),
    log_debug(['GetDateCmd: ', GetDateCmd]),

    shell(GetDateCmd, GetDateStatus),
    log_debug(['GetDateStatus: ', GetDateStatus]),
    ( GetDateStatus \= 0
    ->  throw(unexpected_command_exit_code('Failed to get date'))
    ;   true ),

    open(TempFile, read, Stream, [type(text)]),
    read_stream(Stream, [], Iso8601Date),

    WarningMessage = " || echo 'Removed temporary file already' 1>&2",
    append(["test -e ", TempFile," && rm -f ", TempFile, WarningMessage], RemoveTempFileCmd),
    shell(RemoveTempFileCmd, RemoveTempFileStatus),
    ( RemoveTempFileStatus \= 0
    ->  throw(unexpected_command_exit_code('Failed to remove temporary file'))
    ;   true ).
