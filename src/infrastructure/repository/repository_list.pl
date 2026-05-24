:- module(repository_list, [
    by_list_uri/2,
    by_list_uri_or_throw/2,
    count/1,
    insert/2,
    next_event_id/1,
    next_id/1,
    query/1
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

:- use_module('repository_dcgs', [
    rows//1,
    to_json/3
]).
:- use_module('../pg/client', [
    execute/2,
    matching_criteria/2,
    query_result/2,
    query_result_from_file/3,
    query_result_from_file/4,
    read_rows/2,
    value/3
]).
:- use_module('../../configuration', [pg_backend/1]).
:- use_module('../../logger', [
    log_debug/1,
    log_info/1
]).
:- use_module('../../os_ext', [remove_temporary_file/1]).
:- use_module('../../serialization', [
    pairs_to_assoc/2,
    to_json_chars/2
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
tables. All reads go through `value/3` /
`query_result_from_file/4`; all writes go through `execute/2`
with bind parameters, so no caller-controlled value is ever
concatenated into SQL.
*/

%% table(-Table)
table("publishers_list").

%% event_table(-EventTable).
event_table("publishers_list_collected_event").

%% count(-Count)
%
% Total number of rows in `publishers_list`. Dispatches on PG_BACKEND.
count(Count) :-
    pg_backend("wire"),
    count_sql(SQL),
    value(SQL, [], Count).
count(Count) :-
    pg_backend("psql"),
    count_sql(SQL),
    query_result(SQL, Count).

count_sql(SQL) :-
    table(Table),
    append(["SELECT count(*) AS matching_records_count FROM public.", Table], SQL).

%% query(-HeadersAndRows)
%
% All `publishers_list` rows as a list of header-keyed assocs.
query(HeadersAndRows) :-
    headers(Headers),
    query(Rows, [], "ALL"),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

headers([
    "string__name",
    "string__username",
    "number__list_id",
    "string__public_id",
    "string__created_at"
]).

%% all_clauses(-SQL, -Params, +Limit).
%
% Builds the listing SQL. Limit is either the chars "ALL" (kept as
% literal SQL — PostgreSQL cannot bind keywords) or an integer (bound
% as $1).
all_clauses(SQL, [], "ALL") :-
    all_clauses_template("ALL", SQL).
all_clauses(SQL, [LimitChars], Limit) :-
    integer_si(Limit),
    number_chars(Limit, LimitChars),
    all_clauses_template("$1", SQL).

all_clauses_template(LimitClause, SQL) :-
    table(Table),
    append(
        [
            "SELECT ",
            "t.name::text AS string__name, ",
            "COALESCE(t.screen_name::text, ' ') AS string__username, ",
            "COALESCE(t.list_id::bigint, 0) AS number__list_id, ",
            "t.public_id::uuid AS string__public_id, ",
            "t.created_at AS string__created_at ",
            "FROM public.", Table, " t ",
            "ORDER BY t.public_id DESC, t.name ASC, t.screen_name ASC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        SQL
    ).

%% query(-Rows, +Headers, +Limit).
query(Rows, Headers, Limit) :-
    pg_backend("wire"),
    once(all_clauses(SQL, Params, Limit)),
    once(query_result_from_file(SQL, Params, Headers, TmpFile)),
    read_rows(TmpFile, Rows).
query(Rows, _Headers, Limit) :-
    pg_backend("psql"),
    psql_all_clauses_sql(SQL, Limit),
    once(query_result_from_file(SQL, true, TmpFile)),
    read_rows(TmpFile, Rows).

%% psql_all_clauses_sql(-SQL, +Limit).
%
% Concat form of the listing SELECT for the psql backend. Limit is
% either the chars "ALL" or an integer that gets number_chars/2 into
% the SQL text.
psql_all_clauses_sql(SQL, "ALL") :-
    psql_all_clauses_template("ALL", SQL).
psql_all_clauses_sql(SQL, Limit) :-
    integer_si(Limit),
    number_chars(Limit, LimitChars),
    psql_all_clauses_template(LimitChars, SQL).

psql_all_clauses_template(LimitClause, SQL) :-
    table(Table),
    append(
        [
            "SELECT ",
            "t.name::text AS string__name, ",
            "COALESCE(t.screen_name::text, ' ') AS string__username, ",
            "COALESCE(t.list_id::bigint, 0) AS number__list_id, ",
            "t.public_id::uuid AS string__public_id, ",
            "t.created_at AS string__created_at ",
            "FROM public.", Table, " t ",
            "ORDER BY t.public_id DESC, t.name ASC, t.screen_name ASC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        SQL
    ).

%% next_id(-NextId)
%
% Next available primary key for `publishers_list`: the
% current `MAX(id) + 1`, or `1` if the table is empty.
% Dispatches on PG_BACKEND.
next_id(NextId) :-
    pg_backend("wire"),
    query_max_id_sql(SQL),
    value(SQL, [], MaxIdValue),
    coerce_no_records(MaxIdValue, MaxId),
    NextId #= MaxId + 1,
    validate_id(NextId).
next_id(NextId) :-
    pg_backend("psql"),
    query_max_id_sql(SQL),
    query_result(SQL, MaxIdValue),
    coerce_no_records(MaxIdValue, MaxId),
    NextId #= MaxId + 1,
    validate_id(NextId).

%% next_event_id(-NextEventId)
%
% Next available `list_id` for
% `publishers_list_collected_event`. Dispatches on PG_BACKEND.
next_event_id(NextEventId) :-
    pg_backend("wire"),
    query_event_max_id_sql(SQL),
    value(SQL, [], EventMaxIdValue),
    coerce_no_records(EventMaxIdValue, EventMaxId),
    NextEventId #= EventMaxId + 1,
    validate_id(NextEventId).
next_event_id(NextEventId) :-
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

query_max_id_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT COALESCE(r.id::bigint, 0) pk ",
            "FROM ", Table, " r ",
            "ORDER BY r.id DESC ",
            "LIMIT 1;"
        ],
        SQL
    ).

query_event_max_id_sql(SQL) :-
    event_table(EventTable),
    append(
        [
            "SELECT COALESCE(e.list_id::bigint, 0) list ",
            "FROM public.", EventTable, " e ",
            "ORDER BY list DESC ",
            "LIMIT 1;"
        ],
        SQL
    ).

%% by_list_uri_or_throw(+MainListAtUri, -Assoc)
%
% Read the first event row for `MainListAtUri`, base64-decode
% its JSON payload, and merge in the persisted `list_id`.
% Throws `cannot_find_publishers_list_by_uri` when no row
% matches.
by_list_uri_or_throw(list_uri(MainListAtUri), Assoc) :-
    once(by_list_uri_or_throw_attempt(MainListAtUri, Assoc)).

by_list_uri_or_throw_attempt(MainListAtUri, Assoc) :-
    by_list_uri(MainListAtUri, ListURIRows),
    writeln(rows_selected_by_list_uri(ListURIRows)),
    nth0(0, ListURIRows, FirstListURIRow),
    get_assoc(payload, FirstListURIRow, FirstListURIRowPayload),
    get_assoc(list_id, FirstListURIRow, ListId),
    chars_base64(Utf8BytesPayload, FirstListURIRowPayload, []),
    maplist(char_code, Utf8BytesPayload, Utf8Bytes),
    chars_utf8bytes(PayloadChars, Utf8Bytes),
    append([Prefix, ['"']], PayloadChars),
    append([['"'], Suffix], Prefix),
    to_json_chars(Suffix, JSONChars),
    phrase(json_chars(pairs(Pairs)), JSONChars, []),
    log_debug(pairs: Pairs),
    pairs_to_assoc(Pairs, AnonymousAssoc),
    writeln(anonymous_assoc:AnonymousAssoc),
    put_assoc(list_id, AnonymousAssoc, ListId, Assoc).
by_list_uri_or_throw_attempt(_, _) :-
    throw(cannot_find_publishers_list_by_uri).

%% by_list_uri(+ListURI, -HeadersAndRows)
%
% Multi-row read of `publishers_list_collected_event` for the
% given list AT-URI; each row is returned as a header-keyed
% assoc. Dispatches on PG_BACKEND.
by_list_uri(ListURI, HeadersAndRows) :-
    pg_backend("wire"),
    chars_si(ListURI),
    by_list_uri_headers(Headers),
    by_list_uri_sql(SQL),
    writeln(by_list_URI:SQL),
    once(query_result_from_file(SQL, [ListURI], [], TmpFile)),
    once(read_rows_or_throw(TmpFile, Rows)),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).
by_list_uri(ListURI, HeadersAndRows) :-
    pg_backend("psql"),
    chars_si(ListURI),
    psql_by_list_uri_sql(ListURI, SQL),
    writeln(by_list_URI:SQL),
    matching_criteria(SQL, HeadersAndRows).

%% psql_by_list_uri_sql(+ListURI, -SQL).
%
% Concat form: ListURI is inlined as a single-quoted SQL literal,
% same convention as the pre-69f3f97 client. Trailing semicolon is
% omitted because matching_criteria/2 appends "LIMIT 0;" then
% "LIMIT ALL;" in its two-pass header/rows shape.
psql_by_list_uri_sql(ListURI, SQL) :-
    event_table(EventTable),
    append([
        "SELECT ",
        "payload AS string__payload, ",
        "list_id AS number__list_id, ",
        "list_name AS string__list_name ",
        "FROM public.", EventTable, " e ",
        "WHERE e.occurred_at::date > '2024-12-31'::date ",
        "AND e.list_name = '", ListURI, "' ",
        "OFFSET 0 "
    ], SQL).

read_rows_or_throw(TmpFile, Rows) :- read_rows(TmpFile, Rows).
read_rows_or_throw(_, _) :- throw(cannot_read_rows_selected_by(list_id)).

by_list_uri_headers([
    "string__payload",
    "number__list_id",
    "string__list_name"
]).

by_list_uri_sql(SQL) :-
    event_table(EventTable),
    append([
        "SELECT ",
        "payload AS string__payload, ",
        "list_id AS number__list_id, ",
        "list_name AS string__list_name ",
        "FROM public.", EventTable, " e ",
        "WHERE e.occurred_at::date > '2024-12-31'::date ",
        "AND e.list_name = $1 ",
        "OFFSET 0 LIMIT ALL;"
    ], SQL).

%% count_matching_records(+NextId, -Result). Dispatches on PG_BACKEND.
count_matching_records(NextId, Result) :-
    pg_backend("wire"),
    count_matching_records_sql(SQL),
    number_chars(NextId, NextIdChars),
    value(SQL, [NextIdChars], Result).
count_matching_records(NextId, Result) :-
    pg_backend("psql"),
    number_chars(NextId, NextIdChars),
    psql_count_matching_records_sql(NextIdChars, SQL),
    query_result(SQL, Result).

psql_count_matching_records_sql(NextIdChars, SQL) :-
    event_table(EventTable),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", EventTable, " ",
        "WHERE list_id::bigint = ", NextIdChars, ";"
    ], SQL).

count_matching_records_sql(SQL) :-
    event_table(EventTable),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", EventTable, " ",
        "WHERE list_id::bigint = $1;"
    ], SQL).

%% insert(+Row, -InsertionResult)
%
% Insert a new `publishers_list_collected_event` row, or log
% the existing payload if a row with the matching `list_id`
% already exists. `InsertionResult` is always `ok`.
insert(row(ListName, Payload), InsertionResult) :-
    next_id(NextId),
    count_matching_records(NextId, TotalMatchingRecords),
    if_(
        dif(1, TotalMatchingRecords),
        insert_new(NextId, ListName, Payload, InsertionResult),
        report_existing(NextId, InsertionResult)
    ).

insert_new(NextId, ListName, Payload, ok) :-
    pg_backend("wire"),
    uuidv4_string(NextPrimaryKey),
    number_chars(NextId, NextIdChars),
    insert_event_sql(SQL),
    execute(SQL, [NextPrimaryKey, NextIdChars, ListName, Payload]).
insert_new(NextId, ListName, Payload, ok) :-
    pg_backend("psql"),
    uuidv4_string(NextPrimaryKey),
    number_chars(NextId, NextIdChars),
    psql_insert_event_sql(NextPrimaryKey, NextIdChars, ListName, Payload, SQL),
    query_result(SQL, _Result).

%% psql_insert_event_sql(+NextPrimaryKey, +NextIdChars, +ListName,
%%                       +Payload, -SQL).
%
% Concatenated INSERT for the psql backend. Identifiers are inlined
% as single-quoted SQL literals, the same convention as the
% pre-69f3f97 client. No RETURNING; the caller treats success/failure
% via query_result/2's ok | no_records_found contract.
psql_insert_event_sql(NextPrimaryKey, NextIdChars, ListName, Payload, SQL) :-
    event_table(EventTable),
    append(
        [
            "INSERT INTO public.", EventTable, " (",
            "id, list_id, list_name, payload, occurred_at, started_at, ended_at",
            ") VALUES (",
            "'", NextPrimaryKey, "', ",
            " ", NextIdChars, ", ",
            "'", ListName, "', ",
            "'", Payload, "', ",
            "NOW(), NOW(), NOW()",
            ");"
        ],
        SQL
    ).

report_existing(NextId, ok) :-
    pg_backend("wire"),
    number_chars(NextId, NextIdChars),
    select_payload_sql(SelectSQL),
    value(SelectSQL, [NextIdChars], SelectionResult),
    log_existing_payload(SelectionResult).
report_existing(NextId, ok) :-
    pg_backend("psql"),
    number_chars(NextId, NextIdChars),
    psql_select_payload_sql(NextIdChars, SelectSQL),
    query_result(SelectSQL, SelectionResult),
    log_existing_payload(SelectionResult).

psql_select_payload_sql(NextIdChars, SQL) :-
    event_table(EventTable),
    append(
        [
            "SELECT l.payload AS payload ",
            "FROM public.", EventTable, " l ",
            "WHERE l.list_id::bigint = ", NextIdChars, ";"
        ],
        SQL
    ).

log_existing_payload(no_records_found) :-
    log_info([no_existing_list_payload]).
log_existing_payload(SelectionResult) :-
    dif(SelectionResult, no_records_found),
    chars_base64(DecodedBase64Payload, SelectionResult, []),
    maplist(char_code, DecodedBase64Payload, DecodedBytes),
    chars_utf8bytes(DecodedChars, DecodedBytes),
    append([Prefix, ['"']], DecodedChars),
    append([['"'], Suffix], Prefix),
    to_json_chars(Suffix, JSONChars),
    log_info([JSONChars]).

insert_event_sql(SQL) :-
    event_table(EventTable),
    append(
        [
            "INSERT INTO public.", EventTable, " (",
            "id, list_id, list_name, payload, occurred_at, started_at, ended_at",
            ") VALUES ($1, $2, $3, $4, NOW(), NOW(), NOW())"
        ],
        SQL
    ).

select_payload_sql(SQL) :-
    event_table(EventTable),
    append(
        [
            "SELECT l.payload AS payload ",
            "FROM public.", EventTable, " l ",
            "WHERE l.list_id::bigint = $1;"
        ],
        SQL
    ).
