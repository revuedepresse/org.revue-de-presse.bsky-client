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
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(files)).
:- use_module(library(pio)).
:- use_module(library(reif)).
:- use_module(library(si)).

:- use_module('../repository/repository_dcgs', [
    rows//1,
    to_json/3
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

:- use_module('connection', [pg_query_simple/2]).

%% encode_field_value(+FieldValue, -EncodedFieldValue)
%
% Legacy serializer kept for the existing INSERT-by-string-concat paths.
% New hot-path callers pass values through bind parameters and do not
% need this.
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
%
% Executes Query through the wire client and adapts the response to the
% legacy contract:
%   - integer       if the first cell of the first row is all digits.
%   - chars         for any other non-empty single-cell result.
%   - no_records_found  if the query returned no rows or no result.
%   - ok            for non-row-producing statements.
query_result(Query, Result) :-
    writeln(running_query(Query)),
    pg_query_simple(Query, Reply),
    adapt_simple_result(Reply, Result).

adapt_simple_result(ok, ok) :- !.
adapt_simple_result([], no_records_found) :- !.
adapt_simple_result(data(_Headers, []), no_records_found) :- !.
adapt_simple_result(data(_Headers, [[Cs|_]|_]), Result) :-
    !,
    (   Cs == null
    ->  Result = no_records_found
    ;   ( maplist(is_digit, Cs)
        ->  number_chars(Result, Cs)
        ;   Result = Cs ) ).
adapt_simple_result(error(Err), _) :-
    throw(pg_error(Err)).

%% query_result_from_file(+Query, +TuplesOnly, -TempFile).
%
% Runs Query through the wire client and writes the result rows to a
% tempfile in the legacy pipe-delimited format expected by
% repository_dcgs:rows//1, so the existing read_rows/2 callers don't
% need to change. When TuplesOnly is false, the header row is included
% as the first line.
query_result_from_file(Query, TuplesOnly, TempFile) :-
    writeln(running_query(Query)),
    pg_query_simple(Query, Reply),
    extract_headers_and_rows(Reply, Headers, Rows),
    temporary_file("query", TempFile),
    once(open(TempFile, write, OutStream, [type(text)])),
    (   TuplesOnly == false
    ->  write_piped_row(OutStream, Headers)
    ;   true ),
    maplist(write_piped_row(OutStream), Rows),
    close(OutStream).

extract_headers_and_rows(ok, [], []).
extract_headers_and_rows([], [], []).
extract_headers_and_rows(data(Headers, Rows), Headers, Rows).
extract_headers_and_rows(error(Err), _, _) :-
    throw(pg_error(Err)).

write_piped_row(Stream, Fields) :-
    write_fields_piped(Stream, Fields),
    format(Stream, "~n", []).

write_fields_piped(_, []).
write_fields_piped(Stream, [F]) :- !,
    write_field(Stream, F).
write_fields_piped(Stream, [F|Fs]) :-
    write_field(Stream, F),
    format(Stream, "|", []),
    write_fields_piped(Stream, Fs).

write_field(Stream, null) :- !,
    format(Stream, "", []).
write_field(Stream, Cs) :-
    format(Stream, "~s", [Cs]).

%% matching_criteria(+Criteria, -HeadersAndRows).
%
% Direct path: one simple-protocol query returning headers + rows in
% the wire client's structured shape; convert to the assoc form
% repositories consume.
matching_criteria(Criteria, HeadersAndRows) :-
    append([Criteria, "LIMIT ALL;"], SQL),
    writeln(selection_query(SQL)),
    pg_query_simple(SQL, Reply),
    extract_headers_and_rows(Reply, Headers, Rows),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

%% read_rows(+TmpFile, -Rows).
%
% Parses a tempfile (written by query_result_from_file/3) into Rows
% via the existing repository_dcgs:rows//1 DCG.
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
