:- module(temporal, [
    date_iso8601/1,
    date_iso8601_days_ago/2,
    date_iso8601_days_before/3
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

check_shell_status(0, _).
check_shell_status(Status, Msg) :-
    Status \= 0,
    throw(unexpected_command_exit_code(Msg)).

% See [date command --iso-8601 option](https://unix.stackexchange.com/a/629504)
% date_iso8601(-Date).
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

% date_iso8601_days_ago(+DaysAgo, -Iso8601Date).
%
% Returns the current UTC date/time minus DaysAgo days, in ISO-8601
% (YYYY-MM-DDTHH:MM:SSZ). Tries GNU `date -d 'N days ago'` first and
% falls back to BSD/macOS `date -v -Nd`, so it works in both prod
% (Debian, see Dockerfile) and local dev (macOS).
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

% date_iso8601_days_before(+DaysBefore, +AnchorIso, -ResultDate).
%
% Returns AnchorIso (an ISO-8601 timestamp; only the leading
% YYYY-MM-DD is consumed) minus DaysBefore days, formatted as
% YYYY-MM-DD. Same GNU/BSD fallback as date_iso8601_days_ago/2.
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
