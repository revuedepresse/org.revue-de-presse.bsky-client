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

:- use_module('repository_status_queries', [
    all_clauses/3,
    by_criteria_sql/1,
    by_hash_sql/1,
    by_indexed_at_sql/1,
    count_matching_records_sql/1,
    count_sql/1,
    psql_all_clauses_sql/2,
    psql_by_criteria_sql/2,
    psql_by_hash_sql/2,
    psql_by_indexed_at_sql/3,
    psql_count_matching_records_sql/2,
    psql_status_insert_sql/8,
    psql_status_lookup_by_hash_sql/2,
    query_max_id_sql/1,
    status_insert_sql/1,
    status_lookup_by_hash_sql/1
]).
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
