:- module(temporal, [
    date_iso8601/1,
    date_iso8601_days_ago/2,
    date_iso8601_days_before/3
]).

/**
ISO-8601 date helpers.

Each predicate shells out to `date(1)` because Scryer has no
built-in for `%Y-%m-%dT%H:%M:%SZ` formatting. The command line
tries the GNU `date -d` form first and falls back to the BSD
`date -v` form, so the same predicate works under Debian (the
production image) and macOS (local development).
*/

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

check_shell_status(0, _).
check_shell_status(Status, Msg) :-
    Status \= 0,
    throw(unexpected_command_exit_code(Msg)).

%% date_iso8601(-Iso8601Date)
%
% Current UTC date and time as char list
% `YYYY-MM-DDTHH:MM:SSZ`. See [the date command
% `--iso-8601` option](https://unix.stackexchange.com/a/629504).
date_iso8601(Iso8601Date) :-
    temporary_file("date-iso-8601", TempFile),

    append(["touch ", TempFile, "; echo -n $(date -u +%Y-%m-%dT%H:%M:%SZ) > ", TempFile], GetDateCmd),
    log_debug(['GetDateCmd: ', GetDateCmd]),

    shell(GetDateCmd, GetDateStatus),
    log_debug(['GetDateStatus: ', GetDateStatus]),
    check_shell_status(GetDateStatus, 'Failed to get date'),

    open(TempFile, read, Stream, [type(text)]),
    read_stream(Stream, Iso8601Date),

    remove_temporary_file(TempFile).

%% date_iso8601_days_ago(+DaysAgo, -Iso8601Date)
%
% Current UTC date/time minus `DaysAgo` days, formatted as
% `YYYY-MM-DDTHH:MM:SSZ`. Tries GNU `date -d 'N days ago'`
% first and falls back to BSD/macOS `date -v -Nd`, so it works
% in both prod (Debian, see Dockerfile) and local dev (macOS).
date_iso8601_days_ago(DaysAgo, Iso8601Date) :-
    temporary_file("date-iso-8601-days-ago", TempFile),

    number_chars(DaysAgo, DaysAgoChars),
    append([
        "touch ", TempFile,
        "; echo -n $(",
        "date -u -d '", DaysAgoChars, " days ago' +%Y-%m-%dT%H:%M:%SZ 2>/dev/null",
        " || ",
        "date -u -v -", DaysAgoChars, "d +%Y-%m-%dT%H:%M:%SZ",
        ") > ", TempFile
    ], GetDateCmd),
    log_debug(['GetDateCmd: ', GetDateCmd]),

    shell(GetDateCmd, GetDateStatus),
    log_debug(['GetDateStatus: ', GetDateStatus]),
    check_shell_status(GetDateStatus, 'Failed to get date N days ago'),

    open(TempFile, read, Stream, [type(text)]),
    read_stream(Stream, Iso8601Date),

    remove_temporary_file(TempFile).

%% date_iso8601_days_before(+DaysBefore, +AnchorIso, -ResultDate)
%
% `AnchorIso` minus `DaysBefore` days, formatted as
% `YYYY-MM-DD`. Only the leading `YYYY-MM-DD` of the anchor is
% read, so the predicate accepts any ISO-8601 timestamp prefix.
% Same GNU-then-BSD fallback as [[temporal#date_iso8601_days_ago]].
date_iso8601_days_before(DaysBefore, AnchorIso, ResultDate) :-
    length(AnchorDateChars, 10),
    append(AnchorDateChars, _, AnchorIso),

    temporary_file("date-iso-8601-days-before", TempFile),

    number_chars(DaysBefore, DaysChars),
    append([
        "touch ", TempFile,
        "; echo -n $(",
        "date -u -d '", AnchorDateChars, " - ", DaysChars, " days' +%Y-%m-%d 2>/dev/null",
        " || ",
        "date -u -j -v -", DaysChars, "d -f '%Y-%m-%d' '", AnchorDateChars, "' +%Y-%m-%d",
        ") > ", TempFile
    ], GetDateCmd),
    log_debug(['GetDateCmd: ', GetDateCmd]),

    shell(GetDateCmd, GetDateStatus),
    log_debug(['GetDateStatus: ', GetDateStatus]),
    check_shell_status(GetDateStatus, 'Failed to compute date N days before anchor'),

    open(TempFile, read, Stream, [type(text)]),
    read_stream(Stream, ResultDate),

    remove_temporary_file(TempFile).
