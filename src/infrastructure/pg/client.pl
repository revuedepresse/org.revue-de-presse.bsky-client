:- module(client, [
    query_result_from_file/3,
    query_result/2
]).

:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(si)).
:- use_module(library(reif)).

:- use_module('../../configuration', [
    database_db_name/1,
    database_username/1,
    database_port/1,
    database_password/1,
    database_host/1
]).
:- use_module('../../logger', [
    log_debug/1,
    log_info/1
]).
:- use_module('../../os_ext', [
    remove_temporary_file/1,
    temporary_file/2
]).
:- use_module('../../stream', [
    read_stream/2
]).

%% is_digit(+Char).
is_digit(Char) :-
    char_type(Char, numeric).

%% query_result(+Query, -Result).
query_result(Query, Result) :-
    RemoveResultFile = false,
    query(Query, RemoveResultFile, true, TempFile, Result),
    remove_temporary_file(TempFile).

%% query_result_from_file(+Query, +TuplesOnly, -TempFile).
query_result_from_file(Query, TuplesOnly, TempFile) :-
    RemoveResultFile = false,
    query(Query, RemoveResultFile, TuplesOnly, TempFile, _Result).

%% query(+Query, +RemoveResultFile, +TuplesOnly, -TempFile, -Result).
query(Query, RemoveResultFile, TuplesOnly, TempFile, Result) :-
    database_db_name(DbName),
    database_username(Username),
    database_port(Port),
    database_password(Password),
    database_host(Host),

    temporary_file("query_before_execution", QueryFile),
    once(open(QueryFile, write, QueryFileStream, [type(text)])),
    write_term(QueryFileStream, Query, [double_quotes(true)]),

    char_code(AntiSlash, 92),
    char_code(DoubleQuote, 34),
    temporary_file("query", TempFile),

    if_(
        TuplesOnly = true,
        TuplesOnlyOption = "--tuples-only ",
        TuplesOnlyOption = ""
    ),

    (   append([
            [AntiSlash], "cat ", QueryFile, " | ",
            % Removing leading double quotes
            "sed -E 's#^", [DoubleQuote], "##g' | ",
            % Removing trailing double quotes
            "sed -E 's#", [DoubleQuote], "$##g' | ",
            "PGPASSWORD='", Password, "' ",
            "psql ",
            TuplesOnlyOption,
            "--csv ",
            "--dbname='", DbName, "' ",
            "--host='", Host, "' ",
            "--no-align ",
            "--no-readline ",
            "--output='", TempFile, "' ",
            "--port='", Port, "' ",
            "--quiet ",
            "--set ON_ERROR_STOP=1 ",
            "--username='", Username, "' "
        ], QueryCommand)
    ->  true
    ;   throw(cannot_execute_sql_query) ),

    shell(QueryCommand, QueryExecutionStatus),

    log_debug(['query execution status: ', QueryExecutionStatus]),
    (   QueryExecutionStatus \= 0
    ->  throw(unexpected_command_exit_code('Failed to execute query'))
    ;   remove_temporary_file(QueryFile) ),

    open(TempFile, read, Stream, [type(text)]),
    read_stream(Stream, ReadResult),
    char_code(Eol, 10),

    (   append([IntermediateResult, [Eol]], ReadResult)
    ->  true % results list end with EOL
    ;   IntermediateResult = ReadResult ),

    if_(
        dif(IntermediateResult, []),
        (   maplist(is_digit, IntermediateResult)
        ->  number_chars(Result, IntermediateResult)
        ;   Result = IntermediateResult ),
        Result = ok
    ),

    if_(
        RemoveResultFile = true,
        remove_temporary_file(TempFile),
        true
    ).