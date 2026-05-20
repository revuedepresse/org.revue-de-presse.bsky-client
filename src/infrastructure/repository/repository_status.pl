:- module(repository_status, [
    by_criteria/2,
    by_indexed_at/2,
    count/1,
    extract_lookup_ust_id/2,
    id_hash/3,
    insert/3,
    next_id/1,
    query/1
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(crypto)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(files)).
:- use_module(library(lists)).
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
    encode_field_value/2,
    hash/2,
    matching_criteria/4,
    query_result_from_file/4,
    read_rows/2,
    value/3
]).
:- use_module('../pg/connection', [pg_query/3]).
:- use_module('../../logger', [
    log_debug/1,
    log_error/1,
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

/**
Repository for the `weaving_status` table.

Mirrors the structure of `publication` for the legacy
status-keyed pipeline. `insert/3` is the hot path: it uses
`ON CONFLICT (ust_hash) DO NOTHING RETURNING ust_id` and falls
back to a follow-up SELECT on conflict so the caller always
receives a usable `ust_id`, whether the row was new or a
duplicate of an existing observation.
*/

%% table(-Table)
table("weaving_status").

%% count(-Count)
%
% Total number of rows in `weaving_status`.
count(Count) :-
    count_sql(SQL),
    value(SQL, [], Count).

count_sql(SQL) :-
    table(Table),
    append(["SELECT count(*) AS matching_records_count FROM public.", Table], SQL).

%% query(-HeadersAndRows)
%
% Two most recent `weaving_status` rows as header-keyed assocs.
query(HeadersAndRows) :-
    listing_headers(Headers),
    query(Rows, [], 2),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

listing_headers([
    "number__id",
    "string__full_name",
    "string__avatar",
    "string__status_id",
    "boolean__is_published"
]).

%% all_clauses(-SQL, -Params, +Limit).
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
            "r.ust_id::bigint AS number__id, ",
            "r.ust_full_name AS string__full_name, ",
            "r.ust_avatar AS string__avatar, ",
            "r.ust_status_id AS string__status_id, ",
            "to_json(r.is_published) AS boolean__is_published ",
            "FROM public.", Table, " r ",
            "ORDER BY ust_id DESC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        SQL
    ).

%% query(-Rows, +Headers, +Limit).
query(Rows, Headers, Limit) :-
    once(all_clauses(SQL, Params, Limit)),
    writeln(query:SQL, true),
    once(query_result_from_file(SQL, Params, Headers, TmpFile)),
    read_rows(TmpFile, Rows).

%% next_id(-NextId)
%
% Next available `ust_id` for `weaving_status`.
next_id(NextId) :-
    query_max_id_sql(SQL),
    value(SQL, [], MaxIdValue),
    coerce_no_records(MaxIdValue, MaxId),
    NextId #= MaxId + 1,
    validate_id(NextId).

coerce_no_records(no_records_found, 0).
coerce_no_records(V, V) :- dif(V, no_records_found).

validate_id(N) :- integer_si(N).
validate_id(N) :- \+ integer_si(N), throw(invalid_list_id(N)).

query_max_id_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT COALESCE(r.ust_id::bigint, 0) pk ",
            "FROM ", Table, " r ",
            "ORDER BY r.ust_id DESC ",
            "LIMIT 1;"
        ],
        SQL
    ).

%% by_criteria(+Criteria, -HeadersAndRows)
%
% Look up `weaving_status` rows whose `ust_hash` matches the
% `handle(_)-uri(_)` criterion as header-keyed assocs.
by_criteria(handle(Handle)-uri(URI), HeadersAndRows) :-
    log_if_invalid_unique_identifier(Handle, URI),
    hash(handle(Handle)-uri(URI), Hash),
    by_criteria_headers(Headers),
    by_criteria_sql(SQL),
    matching_criteria(SQL, [Hash], Headers, HeadersAndRows).

log_if_invalid_unique_identifier(Handle, URI) :-
    catch(
        once(( chars_si(Handle), chars_si(URI) )),
        E,
        log_error([invalid_unique_identifier(E)])
    ).

by_criteria_headers([
    "number__id",
    "string__full_name",
    "string__avatar",
    "string__status_id",
    "boolean__is_published"
]).

by_criteria_sql(SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.ust_id::bigint AS number__id, ",
        "r.ust_full_name AS string__full_name, ",
        "r.ust_avatar AS string__avatar, ",
        "r.ust_status_id AS string__status_id, ",
        "to_json(r.is_published) AS boolean__is_published ",
        "FROM public.", Table, " r ",
        "WHERE r.ust_hash = $1 ",
        "OFFSET 0;"
    ], SQL).

%% insert(+Row, -InsertionResult, -InsertedRecordId).
%
% Bind-parameter INSERT through the wire client. Idempotent via
% UNIQUE (ust_hash) + ON CONFLICT DO NOTHING RETURNING ust_id.
%
% InsertionResult is one of:
%   - new(UstIdChars)       a row was inserted; ust_id from RETURNING.
%   - duplicate(Hash)       the hash already existed; ust_id fetched
%                           via a follow-up SELECT.
%
% InsertedRecordId is the ust_id of the row (new or pre-existing) as
% a list of chars, ready to be bound back into popularity inserts.
insert(
    row(
        _FullName,
        Handle,
        PreQuotingText,
        Avatar,
        PreEncodingPayload,
        URI,
        CreatedAt
    ),
    InsertionResult,
    InsertedRecordId
) :-
    hash(handle(Handle)-uri(URI), Hash),
    encode_field_value(PreQuotingText, Text),
    encode_field_value(PreEncodingPayload, Payload),
    status_insert_sql(SQL),
    Params = [Hash, Handle, Handle, Text, Avatar, Payload, URI, "dummy_access_token", "true", CreatedAt],
    pg_query(SQL, Params, Reply),
    interpret_status_insert(Reply, Hash, InsertionResult, InsertedRecordId).

status_insert_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "ust_hash, ust_name, ust_full_name, ust_text, ust_avatar, ",
            "ust_api_document, ust_status_id, ust_access_token, is_published, ust_created_at",
            ") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9::boolean, $10) ",
            "ON CONFLICT (ust_hash) DO NOTHING ",
            "RETURNING ust_id::text"
        ],
        SQL
    ).

interpret_status_insert(data([[UstIdChars|_]|_]), _Hash, new(UstIdChars), UstIdChars) :-
    writeln([status_inserted|[UstIdChars]], true).
interpret_status_insert(data([]), Hash, duplicate(Hash), UstIdChars) :-
    writeln([status_duplicate_skipped|[Hash]], true),
    status_lookup_by_hash_sql(LookupSQL),
    pg_query(LookupSQL, [Hash], Reply),
    extract_lookup_ust_id(Reply, UstIdChars).
interpret_status_insert(error(Err), _, _, _) :-
    throw(pg_error(Err)).

extract_lookup_ust_id(data([[UstIdChars|_]|_]), UstIdChars) :-
    chars_si(UstIdChars).
extract_lookup_ust_id(Reply, _) :-
    \+ ( Reply = data([[Chars|_]|_]), chars_si(Chars) ),
    throw(status_lookup_after_conflict_returned(Reply)).

status_lookup_by_hash_sql(SQL) :-
    table(Table),
    append(["SELECT ust_id::text FROM public.", Table, " WHERE ust_hash = $1"], SQL).

%% id_hash(+UniqueIdentifier, -RecordId, -Result).
id_hash(UniqueIdentifier, RecordId, Result) :-
    once(id_hash_attempt(UniqueIdentifier, RecordId, Result)).

id_hash_attempt(UniqueIdentifier, RecordId, true) :-
    catch(
        ( catch(chars_si(UniqueIdentifier), E, throw(invalid_unique_identifier(E))),
          hash(UniqueIdentifier, Hash),
          by_hash(Hash, Assoc),
          get_assoc(id, Assoc, RecordId),
          writeln([found_record__id|[RecordId]], true) ),
        Cause,
        rethrow_unless_no_records(Cause)
    ).
id_hash_attempt(UniqueIdentifier, _, false) :-
    writeln([no_record_found_by_unique_identifier|[UniqueIdentifier]], true).

rethrow_unless_no_records(no_records_found) :- fail.
rethrow_unless_no_records(Cause) :-
    dif(Cause, no_records_found),
    throw(cannot_select_record_by_hash(Cause)).

%% by_hash(+Hash, -Assoc).
by_hash(Hash, Assoc) :-
    by_hash_headers(Headers),
    by_hash_sql(SQL),
    matching_criteria(SQL, [Hash], Headers, HeadersAndRows),
    extract_first_or_throw(HeadersAndRows, Assoc).

extract_first_or_throw([], _) :- throw(no_records_found).
extract_first_or_throw([Row|Rest], Row) :-
    writeln([selection_result_by_hash|[[Row|Rest]]]).

by_hash_headers([
    "number__id",
    "string__name",
    "string__hash",
    "string__full_name",
    "string__text",
    "string__avatar",
    "string__status_id",
    "boolean__is_published",
    "string__created_at"
]).

by_hash_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT ",
            "r.ust_id as number__id, ",
            "r.ust_name as string__name, ",
            "r.ust_hash as string__hash, ",
            "r.ust_full_name as string__full_name, ",
            "r.ust_text as string__text, ",
            "r.ust_avatar as string__avatar, ",
            "r.ust_status_id as string__status_id, ",
            "r.is_published as boolean__is_published, ",
            "r.ust_created_at as string__created_at ",
            "FROM public.", Table, " r ",
            "WHERE r.ust_hash = $1 ",
            "OFFSET 0;"
        ],
        SQL
    ).

%% count_matching_records(+Row, -Result).
count_matching_records(row(Hash), Result) :-
    count_matching_records_sql(SQL),
    value(SQL, [Hash], Result).

count_matching_records_sql(SQL) :-
    table(Table),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", Table, " r ",
        "WHERE ust_hash = $1;"
    ], SQL).

%% by_indexed_at(+Criteria, -HeadersAndRows)
%
% Look up `weaving_status` rows for a given handle and ISO-8601
% `indexed_at` timestamp. The timestamp is rewritten to
% Postgres' `YYYY-MM-DD HH:MM:SS` form before binding.
by_indexed_at(indexed_at(IndexedAt)-handle(Handle), HeadersAndRows) :-
    writeln([step|'by_indexed_at:parse_date'], true),
    length(Prefix, 10),
    length(Suffix, 8),
    append([Prefix, [_], Suffix, _Rest], IndexedAt),
    append([Prefix, " ", Suffix], IndexedAtDate),
    writeln([step|'by_indexed_at:build_sql'], true),
    by_indexed_at_headers(Headers),
    by_indexed_at_sql(SQL),
    writeln([step|'by_indexed_at:matching_criteria'], true),
    matching_criteria(SQL, [Handle, IndexedAtDate], Headers, HeadersAndRows),
    writeln([step|'by_indexed_at:done'], true).

by_indexed_at_headers([
    "number__id",
    "string__full_name",
    "string__avatar",
    "string__status_id",
    "boolean__is_published"
]).

by_indexed_at_sql(SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.ust_id::bigint AS number__id, ",
        "r.ust_full_name AS string__full_name, ",
        "r.ust_avatar AS string__avatar, ",
        "r.ust_status_id AS string__status_id, ",
        "to_json(r.is_published) AS boolean__is_published ",
        "FROM public.", Table, " r ",
        "WHERE r.ust_name = $1 ",
        "AND r.ust_created_at::timestamp = $2::timestamp ",
        "OFFSET 0;"
    ], SQL).
