:- module(temporal, [
    date_iso8601/1
]).

:- use_module(library(os)).
:- use_module(library(lists)).

:- use_module(logger, [
    log_debug/1,
    log_info/1
]).
:- use_module(os_ext, [
    remove_temporary_file/1,
    temporary_file/2
]).
:- use_module(stream, [
    read_stream/2
]).

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
    read_stream(Stream, Iso8601Date),

    remove_temporary_file(TempFile).
