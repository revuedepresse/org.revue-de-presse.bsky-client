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
    query_result_from_file/4,
    read_rows/2,
    value/3
]).
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

%% table(-Table)
table("publishers_list").

%% event_table(-EventTable).
event_table("publishers_list_collected_event").

%% count(-Count).
count(Count) :-
    count_sql(SQL),
    value(SQL, [], Count).

count_sql(SQL) :-
    table(Table),
    append(["SELECT count(*) AS matching_records_count FROM public.", Table], SQL).

%% query(-HeadersAndRows).
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
    once(all_clauses(SQL, Params, Limit)),
    once(query_result_from_file(SQL, Params, Headers, TmpFile)),
    read_rows(TmpFile, Rows).

%% next_id(-NextId).
next_id(NextId) :-
    query_max_id_sql(SQL),
    value(SQL, [], MaxIdValue),
    coerce_no_records(MaxIdValue, MaxId),
    NextId #= MaxId + 1,
    validate_id(NextId).

%% next_event_id(-NextEventId).
next_event_id(NextEventId) :-
    query_event_max_id_sql(SQL),
    value(SQL, [], EventMaxIdValue),
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

%% by_list_uri_or_throw(+MainListAtUri, -Assoc).
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

%% by_list_uri(+ListURI, -HeadersAndRows).
by_list_uri(ListURI, HeadersAndRows) :-
    chars_si(ListURI),
    by_list_uri_headers(Headers),
    by_list_uri_sql(SQL),
    writeln(by_list_URI:SQL),
    once(query_result_from_file(SQL, [ListURI], [], TmpFile)),
    once(read_rows_or_throw(TmpFile, Rows)),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

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

%% count_matching_records(+NextId, -Result).
count_matching_records(NextId, Result) :-
    count_matching_records_sql(SQL),
    number_chars(NextId, NextIdChars),
    value(SQL, [NextIdChars], Result).

count_matching_records_sql(SQL) :-
    event_table(EventTable),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", EventTable, " ",
        "WHERE list_id::bigint = $1;"
    ], SQL).

%% insert(+Row, -InsertionResult).
insert(row(ListName, Payload), InsertionResult) :-
    next_id(NextId),
    count_matching_records(NextId, TotalMatchingRecords),
    if_(
        dif(1, TotalMatchingRecords),
        insert_new(NextId, ListName, Payload, InsertionResult),
        report_existing(NextId, InsertionResult)
    ).

insert_new(NextId, ListName, Payload, ok) :-
    uuidv4_string(NextPrimaryKey),
    number_chars(NextId, NextIdChars),
    insert_event_sql(SQL),
    execute(SQL, [NextPrimaryKey, NextIdChars, ListName, Payload]).

report_existing(NextId, ok) :-
    number_chars(NextId, NextIdChars),
    select_payload_sql(SelectSQL),
    value(SelectSQL, [NextIdChars], SelectionResult),
    log_existing_payload(SelectionResult).

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
