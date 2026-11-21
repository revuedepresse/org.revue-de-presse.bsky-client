:- module(repository_list, [
    by_list_uri/3,
    by_list_uri_or_throw/3,
    count/2,
    insert/3,
    next_event_id/2,
    next_id/2,
    query/2
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(files)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(dif)).
:- use_module(library(reif)).
:- use_module(library(serialization/json)).
:- use_module(library(si)).
:- use_module(library(uuid)).

:- use_module('repository_list_queries', [
    all_clauses/3,
    by_list_uri_sql/1,
    count_matching_records_sql/1,
    count_sql/1,
    insert_event_sql/1,
    psql_all_clauses_sql/2,
    psql_by_list_uri_sql/2,
    psql_count_matching_records_sql/2,
    psql_insert_event_sql/5,
    psql_select_payload_sql/2,
    query_event_max_id_sql/1,
    query_max_id_sql/1,
    select_payload_sql/1
]).
:- use_module('repository_dcgs', [
    rows//1,
    to_json/3
]).
:- use_module('../pg/client', [
    execute/3,
    matching_criteria/2,
    query_result/2,
    query_result_from_file/3,
    query_result_from_file/5,
    read_rows/2,
    value/4
]).
:- use_module('../../configuration', [pg_backend/1]).
:- use_module('../../logger', [
    log_debug/1,
    log_info/1
]).
:- use_module('../../os_ext', [remove_temporary_file/1]).
:- use_module('../../serialization', [
    pairs_to_assoc/2,
    stored_payload_json_chars/2
]).
:- use_module('../../stream', [
    read_stream/2,
    writeln/1,
    writeln/2
]).
:- use_module('../../temporal', [date_iso8601/1]).

/**
Repository for curated lists of Bluesky authors.

Persists list metadata and the most recent payload event in
the `publishers_list` and `publishers_list_collected_event`
tables. All reads go through `value/4` /
`query_result_from_file/5`; all writes go through `execute/3`
with bind parameters, so no caller-controlled value is ever
concatenated into SQL.

Every wire-path predicate takes a `pg_session(In, Out)` compound
as its first argument; psql-path clauses thread the session as a
passthrough (input = output) since the psql shell-out is
stateless.
*/

%% count(+Session, -Count)
%
% Total number of rows in `publishers_list`. Dispatches on PG_BACKEND.
count(pg_session(C0, C), Count) :-
    pg_backend("wire"),
    count_sql(SQL),
    value(pg_session(C0, C), SQL, [], Count).
count(pg_session(C, C), Count) :-
    pg_backend("psql"),
    count_sql(SQL),
    query_result(SQL, Count).

%% query(+Session, -HeadersAndRows)
%
% All `publishers_list` rows as a list of header-keyed assocs.
query(Session, HeadersAndRows) :-
    headers(Headers),
    query(Session, Rows, [], "ALL"),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

headers([
    "string__name",
    "string__username",
    "number__list_id",
    "string__public_id",
    "string__created_at"
]).

%% query(+Session, -Rows, +Headers, +Limit). Dispatches on PG_BACKEND.
query(pg_session(C0, C), Rows, Headers, Limit) :-
    pg_backend("wire"),
    once(all_clauses(SQL, Params, Limit)),
    once(query_result_from_file(pg_session(C0, C), SQL, Params, Headers, TmpFile)),
    read_rows(TmpFile, Rows).
query(pg_session(C, C), Rows, _Headers, Limit) :-
    pg_backend("psql"),
    psql_all_clauses_sql(SQL, Limit),
    once(query_result_from_file(SQL, true, TmpFile)),
    read_rows(TmpFile, Rows).

%% next_id(+Session, -NextId)
%
% Next available primary key for `publishers_list`: the
% current `MAX(id) + 1`, or `1` if the table is empty.
% Dispatches on PG_BACKEND.
next_id(pg_session(C0, C), NextId) :-
    pg_backend("wire"),
    query_max_id_sql(SQL),
    value(pg_session(C0, C), SQL, [], MaxIdValue),
    coerce_no_records(MaxIdValue, MaxId),
    NextId #= MaxId + 1,
    validate_id(NextId).
next_id(pg_session(C, C), NextId) :-
    pg_backend("psql"),
    query_max_id_sql(SQL),
    query_result(SQL, MaxIdValue),
    coerce_no_records(MaxIdValue, MaxId),
    NextId #= MaxId + 1,
    validate_id(NextId).

%% next_event_id(+Session, -NextEventId)
%
% Next available `list_id` for
% `publishers_list_collected_event`. Dispatches on PG_BACKEND.
next_event_id(pg_session(C0, C), NextEventId) :-
    pg_backend("wire"),
    query_event_max_id_sql(SQL),
    value(pg_session(C0, C), SQL, [], EventMaxIdValue),
    coerce_no_records(EventMaxIdValue, EventMaxId),
    NextEventId #= EventMaxId + 1,
    validate_id(NextEventId).
next_event_id(pg_session(C, C), NextEventId) :-
    pg_backend("psql"),
    query_event_max_id_sql(SQL),
    query_result(SQL, EventMaxIdValue),
    coerce_no_records(EventMaxIdValue, EventMaxId),
    NextEventId #= EventMaxId + 1,
    validate_id(NextEventId).

coerce_no_records(no_records_found, 0).
coerce_no_records(V, V) :- dif(V, no_records_found).

validate_id(N) :- integer_si(N).
validate_id(N) :- \+ integer_si(N), throw(invalid_list_id(N)).

%% by_list_uri_or_throw(+Session, +MainListAtUri, -Assoc)
%
% Read the first event row for `MainListAtUri`, base64-decode
% its JSON payload, and merge in the persisted `list_id`.
% Throws `cannot_find_publishers_list_by_uri` when no row
% matches.
by_list_uri_or_throw(Session, list_uri(MainListAtUri), Assoc) :-
    catch(
        once(by_list_uri_or_throw_attempt(Session, MainListAtUri, Assoc)),
        cannot_read_rows_selected_by(list_id),
        throw(cannot_find_publishers_list_by_uri)
    ).

by_list_uri_or_throw_attempt(Session, MainListAtUri, Assoc) :-
    by_list_uri(Session, MainListAtUri, ListURIRows),
    writeln(rows_selected_by_list_uri(ListURIRows)),
    nth0(0, ListURIRows, FirstListURIRow),
    get_assoc(payload, FirstListURIRow, FirstListURIRowPayload),
    get_assoc(list_id, FirstListURIRow, ListId),
    chars_base64(Utf8BytesPayload, FirstListURIRowPayload, []),
    maplist(char_code, Utf8BytesPayload, Utf8Bytes),
    chars_utf8bytes(PayloadChars, Utf8Bytes),
    stored_payload_json_chars(PayloadChars, JSONChars),
    phrase(json_chars(pairs(Pairs)), JSONChars, []),
    log_debug([pairs: Pairs]),
    pairs_to_assoc(Pairs, AnonymousAssoc),
    writeln(anonymous_assoc:AnonymousAssoc),
    put_assoc(list_id, AnonymousAssoc, ListId, Assoc).
by_list_uri_or_throw_attempt(_, _, _) :-
    throw(cannot_find_publishers_list_by_uri).

%% by_list_uri(+Session, +ListURI, -HeadersAndRows)
%
% Multi-row read of `publishers_list_collected_event` for the
% given list AT-URI; each row is returned as a header-keyed
% assoc. Throws `cannot_read_rows_selected_by(list_id)` when no
% event matches, which `event_getList` turns into an insert.
% Dispatches on PG_BACKEND.
by_list_uri(pg_session(C0, C), ListURI, HeadersAndRows) :-
    pg_backend("wire"),
    chars_si(ListURI),
    by_list_uri_headers(Headers),
    by_list_uri_sql(SQL),
    writeln(by_list_URI:SQL),
    once(query_result_from_file(pg_session(C0, C), SQL, [ListURI], [], TmpFile)),
    once(read_rows_or_throw(TmpFile, Rows)),
    rows_or_throw_on_miss(Rows),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).
by_list_uri(pg_session(C, C), ListURI, HeadersAndRows) :-
    pg_backend("psql"),
    chars_si(ListURI),
    psql_by_list_uri_sql(ListURI, SQL),
    writeln(by_list_URI:SQL),
    matching_criteria(SQL, HeadersAndRows),
    rows_or_throw_on_miss(HeadersAndRows).

%% rows_or_throw_on_miss(+Rows)
%
% A query that matches nothing yields `[]` on both backends; turn
% that into the throw callers catch to take their insert path.
rows_or_throw_on_miss([]) :-
    throw(cannot_read_rows_selected_by(list_id)).
rows_or_throw_on_miss([_|_]).

read_rows_or_throw(TmpFile, Rows) :- read_rows(TmpFile, Rows).
read_rows_or_throw(_, _) :- throw(cannot_read_rows_selected_by(list_id)).

by_list_uri_headers([
    "string__payload",
    "number__list_id",
    "string__list_name"
]).

%% count_matching_records(+Session, +ListName, -Result). Dispatches on PG_BACKEND.
count_matching_records(pg_session(C0, C), ListName, Result) :-
    pg_backend("wire"),
    count_matching_records_sql(SQL),
    value(pg_session(C0, C), SQL, [ListName], Result).
count_matching_records(pg_session(C, C), ListName, Result) :-
    pg_backend("psql"),
    psql_count_matching_records_sql(ListName, SQL),
    query_result(SQL, Result).

%% insert(+Session, +Row, -InsertionResult)
%
% Insert a new `publishers_list_collected_event` row for
% `ListName`, or log the existing payload if the list was
% already recorded. A new list takes the next event `list_id`
% (`next_event_id/2`), so no two lists share one. The list's
% `publishers_list` members carry that `list_id`.
% `InsertionResult` is always `ok`.
insert(pg_session(C0, C), row(ListName, Payload), InsertionResult) :-
    count_matching_records(pg_session(C0, C1), ListName, TotalMatchingRecords),
    if_(
        TotalMatchingRecords = 0,
        insert_new(pg_session(C1, C), ListName, Payload, InsertionResult),
        report_existing(pg_session(C1, C), ListName, InsertionResult)
    ).

insert_new(pg_session(C0, C), ListName, Payload, ok) :-
    pg_backend("wire"),
    next_event_id(pg_session(C0, C1), NextId),
    uuidv4_string(NextPrimaryKey),
    number_chars(NextId, NextIdChars),
    insert_event_sql(SQL),
    execute(pg_session(C1, C), SQL, [NextPrimaryKey, NextIdChars, ListName, Payload]).
insert_new(pg_session(C, C), ListName, Payload, ok) :-
    pg_backend("psql"),
    next_event_id(pg_session(C, C), NextId),
    uuidv4_string(NextPrimaryKey),
    number_chars(NextId, NextIdChars),
    psql_insert_event_sql(NextPrimaryKey, NextIdChars, ListName, Payload, SQL),
    query_result(SQL, _Result).

report_existing(pg_session(C0, C), ListName, ok) :-
    pg_backend("wire"),
    select_payload_sql(SelectSQL),
    value(pg_session(C0, C), SelectSQL, [ListName], SelectionResult),
    log_existing_payload(SelectionResult).
report_existing(pg_session(C, C), ListName, ok) :-
    pg_backend("psql"),
    psql_select_payload_sql(ListName, SelectSQL),
    query_result(SelectSQL, SelectionResult),
    log_existing_payload(SelectionResult).

log_existing_payload(no_records_found) :-
    log_info([no_existing_list_payload]).
log_existing_payload(SelectionResult) :-
    dif(SelectionResult, no_records_found),
    chars_base64(DecodedBase64Payload, SelectionResult, []),
    maplist(char_code, DecodedBase64Payload, DecodedBytes),
    chars_utf8bytes(DecodedChars, DecodedBytes),
    stored_payload_json_chars(DecodedChars, JSONChars),
    log_info([JSONChars]).
