:- module(client, [
    %% wire-era (current) -- session-threaded
    encode_field_value/2,
    execute/3,
    hash/2,
    matching_criteria/5,
    pg_query_or_throw/4,
    read_rows/2,
    query_result_from_file/5,
    record_pg_query_failure/3,
    value/4,

    %% psql-era (resurrected for PG_BACKEND=psql fall-back) -- stateless
    matching_criteria/2,
    query_result/2,
    query_result_from_file/3
]).

/**
Postgres client carrying both API surfaces.

Wire-era API takes a `pg_session(In, Out)` compound as the first
argument. The session carries the wire connection through every
DB-touching predicate without needing a module-level cache: the
input slot is the connection used to run the query, the output
slot is the connection the caller should chain into next (same as
input on the happy path, fresh on a reset-and-retry).

Psql-era API is unchanged: every call shells out to `psql(1)`
independently, so there is no per-session state to thread.

Repository predicates select between the two API surfaces by
gating their clauses with `configuration:pg_backend/1`. See
`doc/specs/2026-05-24-pg-backend-switch-design.md`.
*/

:- use_module(library(charsio)).
:- use_module(library(crypto)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(files)).
:- use_module(library(pio)).
:- use_module(library(dif)).
:- use_module(library(os)).
:- use_module(library(reif)).
:- use_module(library(si)).
:- use_module(library(time)).

:- use_module('../repository/repository_dcgs', [
    rows//1,
    to_json/3
]).
:- use_module('../../configuration', [
    database_db_name/1,
    database_host/1,
    database_password/1,
    database_port/1,
    database_username/1
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

:- use_module('connection', [
    pg_query/4,
    open_pg_connection/1,
    close_pg_connection/1
]).

%% encode_field_value(+FieldValue, -EncodedFieldValue)
%
% Base64-encodes a Prolog term's textual representation so it can be
% carried as an opaque text payload through a bind parameter.
encode_field_value(FieldValue, EncodedFieldValue) :-
    write_term_to_chars(FieldValue, [quoted(true), double_quotes(true)], QuotedFieldValue),
    chars_utf8bytes(QuotedFieldValue, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, FieldUtf8Bytes),
    chars_base64(FieldUtf8Bytes, EncodedFieldValue, []).

%% hash(+UniqueIdentifier, -Hash).
hash(handle(Handle)-uri(URI), Hash) :-
    append([Handle, "|", URI], UniqueIdentifier),
    crypto_data_hash(UniqueIdentifier, Hash, [algorithm(sha256)]).

%% digit_t(+Char, ?T).
%
% Reified digit test: T = true if Char is a numeric char, false otherwise.
digit_t(Char, true)  :- char_type(Char, numeric).
digit_t(Char, false) :- \+ char_type(Char, numeric).

%% all_digits_t(+Chars, ?T).
%
% Reified "every char in the list is numeric".
all_digits_t([], true).
all_digits_t([C|Cs], T) :- if_(digit_t(C), all_digits_t(Cs, T), T = false).

%% execute(+Session, +SQL, +Params).
%
% Non-row-producing statement (INSERT/UPDATE/DELETE without RETURNING).
% Uses extended protocol with bind params. Throws on server error.
% Throws unexpected_rows_returned(_) if the server returns rows.
execute(Session, SQL, Params) :-
    pg_query_or_throw(Session, SQL, Params, Reply),
    handle_execute_reply(Reply).

handle_execute_reply(data([])).
handle_execute_reply(data([R|Rs])) :- throw(unexpected_rows_returned(data([R|Rs]))).
handle_execute_reply(error(Err)) :- throw(pg_error(Err)).

%% value(+Session, +SQL, +Params, -Value).
%
% Single-cell SELECT (or INSERT ... RETURNING one column). Adapts the
% first cell of the first row:
%   - no_records_found  if zero rows or the cell is null.
%   - integer           if the cell is all digits.
%   - chars             otherwise.
%
% Throws pg_error(_) on a server error.
value(Session, SQL, Params, Value) :-
    pg_query_or_throw(Session, SQL, Params, Reply),
    adapt_single_value(Reply, Value).

adapt_single_value(data([]), no_records_found).
adapt_single_value(data([[null|_]|_]), no_records_found).
adapt_single_value(data([[Cs|_]|_]), Value) :-
    \+ Cs == null,
    if_(all_digits_t(Cs), number_chars(Value, Cs), Value = Cs).
adapt_single_value(error(Err), _) :- throw(pg_error(Err)).

%% pg_query_or_throw(+pg_session(In, Out), +SQL, +Params, -Reply).
%
% Cut-free recovery dispatcher. Two-phase: try once on the input
% connection; on a wire-level failure (wire_silent_failure/1, the
% single labelled throw the wire library raises for EOF /
% unexpected response shape / etc.), close the input connection,
% open a fresh one, retry once, and propagate the fresh
% connection as the session's output slot. On non-wire errors
% (server-side pg_error and friends), record + re-throw without
% touching the connection.
%
% Clauses of dispatch_pg_error/5 are mutually exclusive on the
% 5th-argument head pattern -- clause 1 matches the literal
% wire_silent_failure(_) functor, clause 2's variable head is
% guarded by dif/2 to exclude that functor. Backtracking from
% clause 1's success therefore cannot fire clause 2.
pg_query_or_throw(pg_session(Conn0, Conn), SQL, Params, Reply) :-
    catch(
        first_attempt(Conn0, SQL, Params, Reply, Conn),
        Err,
        dispatch_pg_error(pg_session(Conn0, Conn), SQL, Params, Reply, Err)
    ).

first_attempt(Conn0, SQL, Params, Reply, Conn0) :-
    pg_query(Conn0, SQL, Params, Reply).

% Wire-failure recovery clause. Head pattern binds the session's Out
% slot to Conn1 (the freshly opened connection) BEFORE the retry, so
% even if the retry throws, the upstream caller's session compound
% still has a valid (open) connection in its output slot.
dispatch_pg_error(pg_session(Conn0, Conn1), SQL, Params, Reply, wire_silent_failure(Detail)) :-
    record_pg_query_failure(SQL, Params, wire_silent_then_reset_and_retry(Detail)),
    close_pg_connection(Conn0),
    open_pg_connection(Conn1),
    catch(
        pg_query(Conn1, SQL, Params, Reply),
        RetryErr,
        ( record_pg_query_failure(SQL, Params, thrown_after_reset(RetryErr)),
          throw(RetryErr) )
    ).
% Non-wire errors: no connection change. Head pattern pg_session(C, C)
% unifies the input and output slots so the upstream caller sees a
% consistent session compound even though we're about to re-throw.
dispatch_pg_error(pg_session(Conn0, Conn0), SQL, Params, _Reply, Err) :-
    dif(Err, wire_silent_failure(_)),
    record_pg_query_failure(SQL, Params, thrown(Err)),
    throw(Err).

%% record_pg_query_failure(+SQL, +Params, +Reason).
%
% Append a self-contained, re-readable Prolog term describing a
% failing pg_query/3 call to
% /tmp/segv-investigation/pg_query_failures.pl. I/O errors here
% are swallowed on purpose: this predicate must never mask the
% original pg_query failure it is documenting.
record_pg_query_failure(SQL, Params, Reason) :-
    catch(
        ( current_time(T),
          phrase(format_time("%Y-%m-%dT%H:%M:%SZ", T), TimeChars),
          open("/tmp/segv-investigation/pg_query_failures.pl",
               append, Stream, [type(text)]),
          write_canonical(Stream,
              pg_query_failure(
                  at(TimeChars),
                  sql(SQL),
                  params(Params),
                  reason(Reason)
              )),
          write(Stream, '.'),
          nl(Stream),
          close(Stream)
        ),
        _,
        true
    ).

%% query_result_from_file(+Session, +SQL, +Params, +Headers, -TempFile).
%
% Extended-protocol multi-row SELECT. Writes rows to a tempfile in the
% pipe-delimited format read_rows/2 consumes. If Headers is a non-empty
% list of header chars, it is written as the first line; pass [] for
% tuples-only output.
query_result_from_file(Session, SQL, Params, Headers, TempFile) :-
    pg_query_or_throw(Session, SQL, Params, Reply),
    extract_rows(Reply, Rows),
    temporary_file("query", TempFile),
    once(open(TempFile, write, OutStream, [type(text)])),
    write_optional_header(OutStream, Headers),
    maplist(write_piped_row(OutStream), Rows),
    close(OutStream).

write_optional_header(_, []).
write_optional_header(Stream, [H|Hs]) :- write_piped_row(Stream, [H|Hs]).

extract_rows(data(Rows), Rows).
extract_rows([], []).
extract_rows(error(Err), _) :- throw(pg_error(Err)).

write_piped_row(Stream, Fields) :-
    write_fields_piped(Stream, Fields),
    format(Stream, "~n", []).

write_fields_piped(_, []).
write_fields_piped(Stream, [F]) :- write_field(Stream, F).
write_fields_piped(Stream, [F, F2 | Fs]) :-
    write_field(Stream, F),
    format(Stream, "|", []),
    write_fields_piped(Stream, [F2 | Fs]).

write_field(Stream, null) :- format(Stream, "", []).
write_field(Stream, [C|Cs]) :- format(Stream, "~s", [[C|Cs]]).
write_field(Stream, []) :- format(Stream, "~s", [[]]).

%% matching_criteria(+Session, +SQL, +Params, +Headers, -HeadersAndRows).
%
% Extended-protocol multi-row SELECT returning each row as an assoc
% keyed by the typed-column names in Headers.
matching_criteria(Session, SQL, Params, Headers, HeadersAndRows) :-
    pg_query_or_throw(Session, SQL, Params, Reply),
    extract_rows(Reply, Rows),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

%% read_rows(+TmpFile, -Rows).
%
% Reads the tempfile produced by query_result_from_file/5 and parses
% it via the repository_dcgs:rows//1 DCG.
read_rows(TmpFile, Rows) :-
    ensure_file_exists(TmpFile),
    once(open(TmpFile, read, Stream, [type(text)])),
    read_stream(Stream, Chars),
    parse_chars(TmpFile, Chars, Rows),
    remove_temporary_file(TmpFile).

ensure_file_exists(TmpFile) :- file_exists(TmpFile).
ensure_file_exists(TmpFile) :- \+ file_exists(TmpFile), throw(file_does_not_exist(TmpFile)).

parse_chars(_, [], []).
parse_chars(TmpFile, [C|Cs], Rows) :- parse_dcg_or_throw(TmpFile, [C|Cs], Rows).

parse_dcg_or_throw(_, Chars, Rows) :- phrase(rows(Rows), Chars).
parse_dcg_or_throw(TmpFile, Chars, _) :-
    \+ phrase(rows(_), Chars),
    throw(cannot_phrase_stream_from(TmpFile)).


%% ----------------------------------------------------------------------
%% PSQL-ERA API (stateless: every call shells out independently)
%%
%% Reached only when PG_BACKEND=psql is set; otherwise the wire-era
%% API above is used.

%% is_digit_psql(+Char).
is_digit_psql(Char) :-
    char_type(Char, numeric).

%% query_result(+Query, -Result).
query_result(Query, Result) :-
    RemoveResultFile = true,
    query_psql(Query, RemoveResultFile, true, _TempFile, Result).

%% query_result_from_file(+Query, +TuplesOnly, -TempFile).
query_result_from_file(Query, TuplesOnly, TempFile) :-
    RemoveResultFile = false,
    query_psql(Query, RemoveResultFile, TuplesOnly, TempFile, _Result).

%% query_psql(+Query, +RemoveResultFile, +TuplesOnly, -TempFile, -Result).
query_psql(Query, RemoveResultFile, TuplesOnly, TempFile, Result) :-
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
            "--dbname='", DbName, "' ",
            "--host='", Host, "' ",
            "--no-align ",
            "--field-separator='|' ",
            "--pset 'null=' ",
            "--pset 'footer=off' ",
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
            "sed -E 's#^", [DoubleQuote], "##g' | ",
            "sed -E 's#", [AntiSlash], [AntiSlash], "##g' | ",
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

    open(TempFile, read, ReadStream, [type(text)]),
    read_stream(ReadStream, ReadResult),

    char_code(Eol, 10),
    (   append([IntermediateResult, [Eol]], ReadResult)
    ->  true
    ;   IntermediateResult = ReadResult ),

    if_(
        dif(IntermediateResult, []),
        (   maplist(is_digit_psql, IntermediateResult)
        ->  number_chars(Result, IntermediateResult)
        ;   Result = IntermediateResult ),
        Result = no_records_found
    ),

    if_(
        RemoveResultFile = true,
        remove_temporary_file(TempFile),
        true
    ).

%% matching_criteria(+Criteria, -HeadersAndRows).
matching_criteria(Criteria, HeadersAndRows) :-
    append([Criteria, "LIMIT 0;"], QueryHeaders),
    once(query_result_from_file(QueryHeaders, false, HeadersOnlyTempFile)),
    read_rows(HeadersOnlyTempFile, HeadersRows),
    nth0(0, HeadersRows, Headers),

    append([Criteria, "LIMIT ALL;"], CriteriaWithoutLimit),
    writeln(selection_query(CriteriaWithoutLimit)),
    once(query_result_from_file(CriteriaWithoutLimit, true, ByCriteriaTemporaryFile)),
    (   read_rows(ByCriteriaTemporaryFile, Rows)
    ->  true
    ;   throw(cannot_read_rows_selection) ),

    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).
