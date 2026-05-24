:- module(client, [
    encode_field_value/2,
    execute/2,
    hash/2,
    matching_criteria/4,
    pg_query_or_throw/3,
    read_rows/2,
    query_result_from_file/4,
    record_pg_query_failure/3,
    value/3
]).

/**
Bind-parameter Postgres client over the wire-protocol connection.

Every repository module talks to Postgres through the four
public verbs here: `execute/2` for INSERT/UPDATE/DELETE
without RETURNING, `value/3` for single-cell SELECTs and
`RETURNING` clauses, `query_result_from_file/4` for multi-row
SELECTs streamed through a pipe-delimited tempfile, and
`matching_criteria/4` for multi-row SELECTs that fold each
row into a typed assoc. SQL placeholders use the extended
protocol's `$1`, `$2`, … form; no caller-controlled value is
ever concatenated into the SQL text.
*/

:- use_module(library(charsio)).
:- use_module(library(crypto)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(files)).
:- use_module(library(pio)).
:- use_module(library(dif)).
:- use_module(library(reif)).
:- use_module(library(si)).
:- use_module(library(time)).

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

:- use_module('connection', [pg_query/3]).

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

%% execute(+SQL, +Params).
%
% Non-row-producing statement (INSERT/UPDATE/DELETE without RETURNING).
% Uses extended protocol with bind params. Throws on server error.
% Throws unexpected_rows_returned(_) if the server returns rows; use
% value/3 or matching_criteria/4 for that.
execute(SQL, Params) :-
    pg_query_or_throw(SQL, Params, Reply),
    handle_execute_reply(Reply).

handle_execute_reply(data([])).
handle_execute_reply(data([R|Rs])) :- throw(unexpected_rows_returned(data([R|Rs]))).
handle_execute_reply(error(Err)) :- throw(pg_error(Err)).

%% value(+SQL, +Params, -Value).
%
% Single-cell SELECT (or INSERT ... RETURNING one column). Adapts the
% first cell of the first row:
%   - no_records_found  if zero rows or the cell is null.
%   - integer           if the cell is all digits.
%   - chars             otherwise.
%
% Throws pg_error(_) on a server error.
value(SQL, Params, Value) :-
    pg_query_or_throw(SQL, Params, Reply),
    adapt_single_value(Reply, Value).

adapt_single_value(data([]), no_records_found).
adapt_single_value(data([[null|_]|_]), no_records_found).
adapt_single_value(data([[Cs|_]|_]), Value) :-
    \+ Cs == null,
    if_(all_digits_t(Cs), number_chars(Value, Cs), Value = Cs).
adapt_single_value(error(Err), _) :- throw(pg_error(Err)).

%% pg_query_or_throw(+SQL, +Params, -Reply).
%
% Wraps pg_query/3 so that the two failure modes of the wire
% client never reach callers as a bare `false`:
%
%   - if pg_query/3 throws, the offending (SQL, Params) pair is
%     recorded and the original exception is re-raised unchanged.
%   - if pg_query/3 silently fails (the symptom of the wire
%     desync / scryer arena guard kicking in -- see
%     deps/scryer-prolog commit b02e0c47), the (SQL, Params) pair
%     is recorded and a labelled throw is raised so the caller's
%     catch/3 can route around the offending row instead of
%     seeing the whole maplist clause fail by surprise.
%
% Both branches persist enough of the input to reconstruct the
% failing call from disk later; see record_pg_query_failure/3.
pg_query_or_throw(SQL, Params, Reply) :-
    catch(pg_query(SQL, Params, Reply), Err,
          ( record_pg_query_failure(SQL, Params, thrown(Err)),
            throw(Err) )),
    !.
pg_query_or_throw(SQL, Params, _) :-
    record_pg_query_failure(SQL, Params, silent),
    throw(pg_query_silently_failed(SQL, Params)).

%% record_pg_query_failure(+SQL, +Params, +Reason).
%
% Append a self-contained, re-readable Prolog term describing a
% failing pg_query/3 call to
% /tmp/segv-investigation/pg_query_failures.pl. The file is the
% same convention used by capture_pre_wire_call/4 in
% repository_status.pl so any tooling that follows
% segv-investigation captures already knows where to look.
%
% Each entry is one valid clause of the form
%
%   pg_query_failure(
%     at(<ISO-8601 chars>),
%     sql(<chars list>),
%     params(<list>),
%     reason(<silent | thrown(...)>)
%   ).
%
% so a downstream reader can `consult/1` the file and walk the
% failures as plain Prolog terms. I/O errors here are swallowed
% on purpose: this predicate must never mask the original
% pg_query failure it is documenting.
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

%% query_result_from_file(+SQL, +Params, +Headers, -TempFile).
%
% Extended-protocol multi-row SELECT. Writes rows to a tempfile in the
% pipe-delimited format read_rows/2 consumes. If Headers is a non-empty
% list of header chars, it is written as the first line; pass [] for
% tuples-only output.
query_result_from_file(SQL, Params, Headers, TempFile) :-
    pg_query_or_throw(SQL, Params, Reply),
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

%% matching_criteria(+SQL, +Params, +Headers, -HeadersAndRows).
%
% Extended-protocol multi-row SELECT returning each row as an assoc
% keyed by the typed-column names in Headers. Headers must match the
% SELECT projection (e.g. ["string__name", "number__list_id", ...]).
matching_criteria(SQL, Params, Headers, HeadersAndRows) :-
    pg_query_or_throw(SQL, Params, Reply),
    extract_rows(Reply, Rows),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

%% read_rows(+TmpFile, -Rows).
%
% Reads the tempfile produced by query_result_from_file/4 and parses
% it via the repository_dcgs:rows//1 DCG. Two cases drive the clausal
% split: an empty file (no rows written) returns Rows = []; a non-empty
% file is fed through the DCG. The tempfile is removed either way.
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
