:- module(client, [
    encode_field_value/2,
    hash/2,
    matching_criteria/2,
    read_rows/2,
    query_result_from_file/3,
    query_result/2
]).

:- use_module(library(charsio)).
:- use_module(library(crypto)).
:- use_module(library(lists)).
:- use_module(library(files)).
:- use_module(library(pio)).
:- use_module(library(os)).
:- use_module(library(si)).
:- use_module(library(reif)).

:- use_module('../repository/repository_dcgs', [
    rows//1,
    to_json/3
]).
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
:- use_module('../../serialization', [
    char_code_at/2,
    pairs_to_assoc/2
]).
:- use_module('../../stream', [
    read_stream/2,
    writeln/1,
    writeln/2
]).

%% encode_field_value(+FieldValue, -EncodedFieldValue)
encode_field_value(FieldValue, EncodedFieldValue) :-
    write_term_to_chars(FieldValue, [quoted(true), double_quotes(true)], QuotedFieldValue),
    chars_utf8bytes(QuotedFieldValue, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, FieldUtf8Bytes),
    chars_base64(FieldUtf8Bytes, EncodedFieldValue, []).

%% hash(+UniqueIdentifier, -Hash).
hash(handle(Handle)-uri(URI), Hash) :-
    append([Handle, "|", URI], UniqueIdentifier),
    crypto_data_hash(UniqueIdentifier, Hash, [algorithm(sha256)]).

%% is_digit(+Char).
is_digit(Char) :-
    char_type(Char, numeric).

%% query_result(+Query, -Result).
query_result(Query, Result) :-
    RemoveResultFile = true,
    query(Query, RemoveResultFile, true, _TempFile, Result).

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
    close(QueryFileStream),

    char_code(AntiSlash, 92),
    char_code(DoubleQuote, 34),
    temporary_file("query", TempFile),

    writeln(creating_temporary_file(TempFile)-for_query(Query)-tuples_only(TuplesOnly)),

    if_(
        TuplesOnly = true,
        TuplesOnlyOption = "--tuples-only ",
        TuplesOnlyOption = ""
    ),

    append(
        [
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
        ],
        CommandSuffix
    ),

    (   append([
            [AntiSlash], "cat ", QueryFile, " | ",
            % Removing leading double quotes
            "sed -E 's#^", [DoubleQuote], "##g' | ",
            % Removing antislashes
            "sed -E 's#", [AntiSlash], [AntiSlash], "##g' | ",
            % Removing trailing double quotes
            "sed -E 's#", [DoubleQuote], "$##g' | ",
            "PGPASSWORD='", Password, "' ",
            CommandSuffix
        ], QueryCommand)
    ->  true
    ;   throw(cannot_execute_sql_query) ),

    shell(QueryCommand, QueryExecutionStatus),

    (   QueryExecutionStatus \= 0
    ->  write_term(cmd:CommandSuffix, [quoted(false),double_quotes(true)]),
        throw(unexpected_command_exit_code('Failed to execute query'))
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
        Result = no_records_found
    ),

    if_(
        RemoveResultFile = true,
        remove_temporary_file(TempFile),
        true
    ).

matching_criteria(Criteria, HeadersAndRows) :-
    append(
        [
            Criteria,
            "LIMIT 0;"
        ],
        QueryHeaders
    ),

    once(query_result_from_file(
        QueryHeaders,
        false,
        HeadersOnlyTempFile
    )),
    read_rows(HeadersOnlyTempFile, HeadersRows),

    nth0(0, HeadersRows, Headers),
    append(
        [
            Criteria,
            "LIMIT ALL;"
        ],
        CriteriaWithoutLimit
    ),
    writeln(selection_query(CriteriaWithoutLimit)),

    once(query_result_from_file(
        CriteriaWithoutLimit,
        true,
        ByCriteriaTemporaryFile
    )),
    (   read_rows(ByCriteriaTemporaryFile, Rows)
    ->  true
    ;   throw(cannot_read_rows_selection) ),

    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

%% read_rows(+TmpFile, -Rows).
read_rows(TmpFile, Rows) :-
    once(open(TmpFile, read, Stream, [type(text)])),
    once((
        file_exists(TmpFile)
        ;   throw(file_does_not_exist)
    )),
    (   once(phrase_from_stream(rows(Rows), Stream))
    ->  remove_temporary_file(TmpFile)
    ;
        once(open(TmpFile, read, StreamBis, [type(text)])),
        read_stream(StreamBis, Chars),
        if_(
            Chars = [],
            remove_temporary_file(TmpFile),
            throw(cannot_phrase_stream_from(TmpFile))
        )
    ).
