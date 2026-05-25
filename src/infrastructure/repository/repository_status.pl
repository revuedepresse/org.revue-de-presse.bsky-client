:- module(repository_status, [
    by_criteria/3,
    by_indexed_at/3,
    count/2,
    exists_by_uri_t/4,
    extract_lookup_ust_id/2,
    id_hash/4,
    insert/4,
    next_id/2,
    query/2
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
    matching_criteria/2,
    matching_criteria/5,
    pg_query_or_throw/4,
    query_result/2,
    query_result_from_file/3,
    query_result_from_file/5,
    read_rows/2,
    value/4
]).
:- use_module('../../configuration', [pg_backend/1]).
:- use_module('../../logger', [
    log_debug/1,
    log_error/1,
    log_info/1
]).
:- use_module('../../clean_text', [clean_text/2]).
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

Every wire-path predicate takes a `pg_session(In, Out)` compound
as its first argument; psql-path clauses thread the session as a
passthrough (input = output) since the psql shell-out is
stateless.

`insert/4` is the hot path: it uses `ON CONFLICT (ust_hash) DO
NOTHING RETURNING ust_id` and falls back to a follow-up SELECT on
conflict so the caller always receives a usable `ust_id`.
*/

%% table(-Table)
table("weaving_status").

%% count(+Session, -Count)
%
% Total number of rows. Dispatches on PG_BACKEND.
count(pg_session(C0, C), Count) :-
    pg_backend("wire"),
    count_sql(SQL),
    value(pg_session(C0, C), SQL, [], Count).
count(pg_session(C, C), Count) :-
    pg_backend("psql"),
    count_sql(SQL),
    query_result(SQL, Count).

count_sql(SQL) :-
    table(Table),
    append(["SELECT count(*) AS matching_records_count FROM public.", Table], SQL).

%% query(+Session, -HeadersAndRows)
%
% Two most recent rows as header-keyed assocs.
query(Session, HeadersAndRows) :-
    listing_headers(Headers),
    query(Session, Rows, [], 2),
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

%% query(+Session, -Rows, +Headers, +Limit). Dispatches on PG_BACKEND.
query(pg_session(C0, C), Rows, Headers, Limit) :-
    pg_backend("wire"),
    once(all_clauses(SQL, Params, Limit)),
    writeln(query:SQL, true),
    once(query_result_from_file(pg_session(C0, C), SQL, Params, Headers, TmpFile)),
    read_rows(TmpFile, Rows).
query(pg_session(C, C), Rows, _Headers, Limit) :-
    pg_backend("psql"),
    psql_all_clauses_sql(SQL, Limit),
    writeln(query:SQL, true),
    once(query_result_from_file(SQL, true, TmpFile)),
    read_rows(TmpFile, Rows).

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

%% next_id(+Session, -NextId)
%
% Next available `ust_id`. Dispatches on PG_BACKEND.
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

%% by_criteria(+Session, +Criteria, -HeadersAndRows)
by_criteria(pg_session(C0, C), handle(Handle)-uri(URI), HeadersAndRows) :-
    pg_backend("wire"),
    log_if_invalid_unique_identifier(Handle, URI),
    hash(handle(Handle)-uri(URI), Hash),
    by_criteria_headers(Headers),
    by_criteria_sql(SQL),
    matching_criteria(pg_session(C0, C), SQL, [Hash], Headers, HeadersAndRows).
by_criteria(pg_session(C, C), handle(Handle)-uri(URI), HeadersAndRows) :-
    pg_backend("psql"),
    log_if_invalid_unique_identifier(Handle, URI),
    hash(handle(Handle)-uri(URI), Hash),
    psql_by_criteria_sql(Hash, SQL),
    matching_criteria(SQL, HeadersAndRows).

psql_by_criteria_sql(Hash, SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.ust_id::bigint AS number__id, ",
        "r.ust_full_name AS string__full_name, ",
        "r.ust_avatar AS string__avatar, ",
        "r.ust_status_id AS string__status_id, ",
        "to_json(r.is_published) AS boolean__is_published ",
        "FROM public.", Table, " r ",
        "WHERE r.ust_hash = '", Hash, "' ",
        "OFFSET 0 "
    ], SQL).

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

%% insert(+Session, +Row, -InsertionResult, -InsertedRecordId).
%
% Bind-parameter INSERT through the wire client. Idempotent via
% UNIQUE (ust_hash) + ON CONFLICT DO NOTHING RETURNING ust_id.
%
% InsertionResult is one of:
%   - new(UstIdChars)       a row was inserted; ust_id from RETURNING.
%   - duplicate(Hash)       the hash already existed; ust_id fetched
%                           via a follow-up SELECT.
insert(
    pg_session(C0, C),
    row(_FullName, Handle, PreQuotingText, Avatar, PreEncodingPayload, URI, CreatedAt),
    InsertionResult,
    InsertedRecordId
) :-
    pg_backend("wire"),
    hash(handle(Handle)-uri(URI), Hash),
    clean_text(PreQuotingText, CleanedText),
    encode_field_value(CleanedText, Text),
    encode_field_value(PreEncodingPayload, Payload),
    status_insert_sql(SQL),
    Params = [Hash, Handle, Handle, Text, Avatar, Payload, URI, "dummy_access_token", "true", CreatedAt],
    pg_query_or_throw(pg_session(C0, C1), SQL, Params, Reply),
    interpret_status_insert(pg_session(C1, C), Reply, Hash, InsertionResult, InsertedRecordId).
insert(
    pg_session(C, C),
    row(_FullName, Handle, PreQuotingText, Avatar, PreEncodingPayload, URI, CreatedAt),
    InsertionResult,
    InsertedRecordId
) :-
    pg_backend("psql"),
    hash(handle(Handle)-uri(URI), Hash),
    clean_text(PreQuotingText, CleanedText),
    encode_field_value(CleanedText, Text),
    encode_field_value(PreEncodingPayload, Payload),
    psql_status_insert_sql(Hash, Handle, Text, Avatar, Payload, URI, CreatedAt, SQL),
    query_result(SQL, Result),
    interpret_psql_status_insert(Result, Hash, InsertionResult, InsertedRecordId).

psql_status_insert_sql(Hash, Handle, EncodedText, Avatar, EncodedPayload, URI, CreatedAt, SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "ust_hash, ust_name, ust_full_name, ust_text, ust_avatar, ",
            "ust_api_document, ust_status_id, ust_access_token, is_published, ust_created_at",
            ") VALUES (",
            "'", Hash, "', ",
            "'", Handle, "', ",
            "'", Handle, "', ",
            "'", EncodedText, "', ",
            "'", Avatar, "', ",
            "'", EncodedPayload, "', ",
            "'", URI, "', ",
            "'dummy_access_token', ",
            "true, ",
            "'", CreatedAt, "'",
            ") ",
            "ON CONFLICT (ust_hash) DO NOTHING ",
            "RETURNING ust_id::text;"
        ],
        SQL
    ).

interpret_psql_status_insert(no_records_found, Hash, duplicate(Hash), UstIdChars) :-
    writeln([status_duplicate_skipped|[Hash]], true),
    psql_status_lookup_by_hash_sql(Hash, LookupSQL),
    query_result(LookupSQL, LookupResult),
    coerce_ust_id_to_chars(LookupResult, UstIdChars).
interpret_psql_status_insert(Result, _Hash, new(UstIdChars), UstIdChars) :-
    dif(Result, no_records_found),
    coerce_ust_id_to_chars(Result, UstIdChars),
    writeln([status_inserted|[UstIdChars]], true).

coerce_ust_id_to_chars(N, Chars) :- integer(N), number_chars(N, Chars).
coerce_ust_id_to_chars(Cs, Cs) :- \+ integer(Cs), Cs \= no_records_found.

psql_status_lookup_by_hash_sql(Hash, SQL) :-
    table(Table),
    append([
        "SELECT ust_id::text FROM public.", Table,
        " WHERE ust_hash = '", Hash, "';"
    ], SQL).

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

%% interpret_status_insert(+Session, +Reply, +Hash, -InsertionResult, -InsertedRecordId).
%
% New ust_id from RETURNING -> new(UstIdChars). No row returned -> the
% INSERT was a conflict and the existing ust_id must be looked up via a
% follow-up SELECT on the same session.
interpret_status_insert(pg_session(C, C), data([[UstIdChars|_]|_]), _Hash, new(UstIdChars), UstIdChars) :-
    writeln([status_inserted|[UstIdChars]], true).
interpret_status_insert(pg_session(C0, C), data([]), Hash, duplicate(Hash), UstIdChars) :-
    writeln([status_duplicate_skipped|[Hash]], true),
    status_lookup_by_hash_sql(LookupSQL),
    pg_query_or_throw(pg_session(C0, C), LookupSQL, [Hash], Reply),
    extract_lookup_ust_id(Reply, UstIdChars).
interpret_status_insert(_Session, error(Err), _, _, _) :-
    throw(pg_error(Err)).

extract_lookup_ust_id(data([[UstIdChars|_]|_]), UstIdChars) :-
    chars_si(UstIdChars).
extract_lookup_ust_id(Reply, _) :-
    \+ ( Reply = data([[Chars|_]|_]), chars_si(Chars) ),
    throw(status_lookup_after_conflict_returned(Reply)).

status_lookup_by_hash_sql(SQL) :-
    table(Table),
    append(["SELECT ust_id::text FROM public.", Table, " WHERE ust_hash = $1"], SQL).

%% id_hash(+Session, +UniqueIdentifier, -RecordId, -Result).
id_hash(Session, UniqueIdentifier, RecordId, Result) :-
    once(id_hash_attempt(Session, UniqueIdentifier, RecordId, Result)).

id_hash_attempt(Session, UniqueIdentifier, RecordId, true) :-
    catch(
        ( catch(chars_si(UniqueIdentifier), E, throw(invalid_unique_identifier(E))),
          hash(UniqueIdentifier, Hash),
          by_hash(Session, Hash, Assoc),
          get_assoc(id, Assoc, RecordId),
          writeln([found_record__id|[RecordId]], true) ),
        Cause,
        rethrow_unless_no_records(Cause)
    ).
id_hash_attempt(_Session, UniqueIdentifier, _, false) :-
    writeln([no_record_found_by_unique_identifier|[UniqueIdentifier]], true).

rethrow_unless_no_records(no_records_found) :- fail.
rethrow_unless_no_records(Cause) :-
    dif(Cause, no_records_found),
    throw(cannot_select_record_by_hash(Cause)).

%% by_hash(+Session, +Hash, -Assoc). Dispatches on PG_BACKEND.
by_hash(pg_session(C0, C), Hash, Assoc) :-
    pg_backend("wire"),
    by_hash_headers(Headers),
    by_hash_sql(SQL),
    matching_criteria(pg_session(C0, C), SQL, [Hash], Headers, HeadersAndRows),
    extract_first_or_throw(HeadersAndRows, Assoc).
by_hash(pg_session(C, C), Hash, Assoc) :-
    pg_backend("psql"),
    psql_by_hash_sql(Hash, SQL),
    matching_criteria(SQL, HeadersAndRows),
    extract_first_or_throw(HeadersAndRows, Assoc).

psql_by_hash_sql(Hash, SQL) :-
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
            "WHERE r.ust_hash = '", Hash, "' ",
            "OFFSET 0 "
        ],
        SQL
    ).

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

psql_count_matching_records_sql(Hash, SQL) :-
    table(Table),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", Table, " r ",
        "WHERE ust_hash = '", Hash, "';"
    ], SQL).

%% exists_by_uri_t(+Session, +Handle, +URI, ?T).
%
% Reified existence check. Pure two-clause dispatch on the count
% replaces the previous Count =:= 0 -> ... ; ... (soft cut).
exists_by_uri_t(Session, Handle, URI, T) :-
    hash(handle(Handle)-uri(URI), Hash),
    count_matching_records_raw(Session, Hash, Count),
    count_to_existence(Count, T).

count_to_existence(0, false).
count_to_existence(N, true) :- N > 0.

count_matching_records_raw(pg_session(C0, C), Hash, Count) :-
    pg_backend("wire"),
    count_matching_records_sql(SQL),
    pg_query_or_throw(pg_session(C0, C), SQL, [Hash], Reply),
    interpret_count_reply(Reply, Count).
count_matching_records_raw(pg_session(C, C), Hash, Count) :-
    pg_backend("psql"),
    psql_count_matching_records_sql(Hash, SQL),
    query_result(SQL, Count).

interpret_count_reply(data([[CountChars|_]|_]), Count) :-
    number_chars(Count, CountChars).
interpret_count_reply(error(Err), _) :-
    throw(pg_error(Err)).

count_matching_records_sql(SQL) :-
    table(Table),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", Table, " r ",
        "WHERE ust_hash = $1;"
    ], SQL).

%% by_indexed_at(+Session, +Criteria, -HeadersAndRows)
by_indexed_at(pg_session(C0, C), indexed_at(IndexedAt)-handle(Handle), HeadersAndRows) :-
    pg_backend("wire"),
    length(Prefix, 10),
    length(Suffix, 8),
    append([Prefix, [_], Suffix, _Rest], IndexedAt),
    append([Prefix, " ", Suffix], IndexedAtDate),
    by_indexed_at_headers(Headers),
    by_indexed_at_sql(SQL),
    catch(
        capture_pre_wire_call(Handle, IndexedAt, IndexedAtDate, SQL),
        _,
        true
    ),
    matching_criteria(pg_session(C0, C), SQL, [Handle, IndexedAtDate], Headers, HeadersAndRows).
by_indexed_at(pg_session(C, C), indexed_at(IndexedAt)-handle(Handle), HeadersAndRows) :-
    pg_backend("psql"),
    length(Prefix, 10),
    length(Suffix, 8),
    append([Prefix, [_], Suffix, _Rest], IndexedAt),
    append([Prefix, " ", Suffix], IndexedAtDate),
    psql_by_indexed_at_sql(Handle, IndexedAtDate, SQL),
    matching_criteria(SQL, HeadersAndRows).

psql_by_indexed_at_sql(Handle, IndexedAtDate, SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.ust_id::bigint AS number__id, ",
        "r.ust_full_name AS string__full_name, ",
        "r.ust_avatar AS string__avatar, ",
        "r.ust_status_id AS string__status_id, ",
        "to_json(r.is_published) AS boolean__is_published ",
        "FROM public.", Table, " r ",
        "WHERE r.ust_name = '", Handle, "' ",
        "AND r.ust_created_at::timestamp = '", IndexedAtDate, "'::timestamp ",
        "OFFSET 0 "
    ], SQL).

capture_pre_wire_call(Handle, IndexedAt, IndexedAtDate, SQL) :-
    Path = "/tmp/segv-investigation/last-by-indexed-at.pl",
    open(Path, write, Stream, [type(text)]),
    write_canonical(Stream, last_by_indexed_at(
        handle(Handle),
        indexed_at(IndexedAt),
        indexed_at_date(IndexedAtDate),
        sql(SQL)
    )),
    write(Stream, '.'),
    nl(Stream),
    close(Stream).

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
