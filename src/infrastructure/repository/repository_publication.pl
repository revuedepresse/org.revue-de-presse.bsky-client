:- module(repository_publication, [
    by_criteria/3,
    count/2,
    insert/3,
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
Repository for the `publication` table.

Every wire-path predicate takes a `pg_session(In, Out)` compound
as its first argument; psql-path clauses thread the session as a
passthrough.
*/

table("publication").

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

query(Session, HeadersAndRows) :-
    listing_headers(Headers),
    query(Session, Rows, [], 2),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

listing_headers([
    "number__id",
    "string__full_name",
    "string__avatar",
    "string__status_id"
]).

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
            "r.legacy_id AS number__id, ",
            "r.screen_name AS string__full_name, ",
            "r.avatar_url AS string__avatar, ",
            "r.document_id AS string__status_id ",
            "FROM public.", Table, " r ",
            "ORDER BY legacy_id DESC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        SQL
    ).

query(pg_session(C0, C), Rows, Headers, Limit) :-
    pg_backend("wire"),
    once(all_clauses(SQL, Params, Limit)),
    writeln(query:SQL, true),
    once(query_result_from_file(pg_session(C0, C), SQL, Params, Headers, TmpFile)),
    read_rows(TmpFile, Rows).
query(pg_session(C, C), Rows, _Headers, Limit) :-
    pg_backend("psql"),
    psql_all_clauses_sql(SQL, Limit),
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
            "r.legacy_id AS number__id, ",
            "r.screen_name AS string__full_name, ",
            "r.avatar_url AS string__avatar, ",
            "r.document_id AS string__status_id ",
            "FROM public.", Table, " r ",
            "ORDER BY legacy_id DESC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        SQL
    ).

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
            "SELECT COALESCE(r.legacy_id::bigint, 0) pk ",
            "FROM ", Table, " r ",
            "ORDER BY r.legacy_id DESC ",
            "LIMIT 1;"
        ],
        SQL
    ).

by_criteria(pg_session(C0, C), handle(Handle)-uri(URI), HeadersAndRows) :-
    pg_backend("wire"),
    hash(handle(Handle)-uri(URI), Hash),
    validate_unique_identifier_chars(Handle, URI),
    by_criteria_headers(Headers),
    by_criteria_sql(SQL),
    matching_criteria(pg_session(C0, C), SQL, [Hash], Headers, HeadersAndRows).
by_criteria(pg_session(C, C), handle(Handle)-uri(URI), HeadersAndRows) :-
    pg_backend("psql"),
    hash(handle(Handle)-uri(URI), Hash),
    validate_unique_identifier_chars(Handle, URI),
    psql_by_criteria_sql(Hash, SQL),
    matching_criteria(SQL, HeadersAndRows).

psql_by_criteria_sql(Hash, SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.legacy_id AS number__id, ",
        "r.screen_name AS string__full_name, ",
        "r.avatar_url AS string__avatar, ",
        "r.document_id AS string__status_id ",
        "FROM public.", Table, " r ",
        "WHERE r.hash = '", Hash, "' ",
        "OFFSET 0 "
    ], SQL).

validate_unique_identifier_chars(Handle, URI) :-
    catch(
        once(( chars_si(Handle), chars_si(URI) )),
        Cause,
        throw(invalid_unique_identifier(Cause))
    ).

by_criteria_headers([
    "number__id",
    "string__full_name",
    "string__avatar",
    "string__status_id"
]).

by_criteria_sql(SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.legacy_id AS number__id, ",
        "r.screen_name AS string__full_name, ",
        "r.avatar_url AS string__avatar, ",
        "r.document_id AS string__status_id ",
        "FROM public.", Table, " r ",
        "WHERE r.hash = $1 ",
        "OFFSET 0;"
    ], SQL).

%% insert(+Session, +Row, -InsertionResult).
insert(
    pg_session(C0, C),
    row(_FullName, Handle, PreQuotingText, Avatar, PreEncodingPayload, URI, CreatedAt, RecordId),
    InsertionResult
) :-
    pg_backend("wire"),
    hash(handle(Handle)-uri(URI), Hash),
    uuidv4_string(PublicId),
    encode_field_value(PreEncodingPayload, Payload),
    clean_text(PreQuotingText, CleanedText),
    encode_field_value(CleanedText, Text),
    publication_insert_sql(SQL),
    Params = [PublicId, RecordId, Hash, Handle, Text, Avatar, URI, Payload, CreatedAt],
    pg_query_or_throw(pg_session(C0, C), SQL, Params, Reply),
    interpret_publication_insert(Reply, Hash, InsertionResult).
insert(
    pg_session(C, C),
    row(_FullName, Handle, PreQuotingText, Avatar, PreEncodingPayload, URI, CreatedAt, RecordId),
    InsertionResult
) :-
    pg_backend("psql"),
    hash(handle(Handle)-uri(URI), Hash),
    uuidv4_string(PublicId),
    encode_field_value(PreEncodingPayload, Payload),
    clean_text(PreQuotingText, CleanedText),
    encode_field_value(CleanedText, Text),
    psql_publication_insert_sql(PublicId, RecordId, Hash, Handle, Text, Avatar, URI, Payload, CreatedAt, SQL),
    query_result(SQL, Result),
    interpret_publication_insert_psql(Result, Hash, InsertionResult).

publication_insert_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "id, legacy_id, hash, screen_name, text, avatar_url, document_id, document, published_at",
            ") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9) ",
            "ON CONFLICT (hash) DO NOTHING ",
            "RETURNING legacy_id"
        ],
        SQL
    ).

psql_publication_insert_sql(PublicId, RecordId, Hash, Handle, Text, Avatar, URI, Payload, CreatedAt, SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "id, legacy_id, hash, screen_name, text, avatar_url, document_id, document, published_at",
            ") VALUES (",
            "'", PublicId, "', ",
            "'", RecordId, "', ",
            "'", Hash, "', ",
            "'", Handle, "', ",
            "'", Text, "', ",
            "'", Avatar, "', ",
            "'", URI, "', ",
            "'", Payload, "', ",
            "'", CreatedAt, "'",
            ") ",
            "ON CONFLICT (hash) DO NOTHING ",
            "RETURNING legacy_id;"
        ],
        SQL
    ).

interpret_publication_insert(data([[LegacyIdChars|_]|_]), _Hash, new(LegacyIdChars)) :-
    writeln([publication_inserted|[LegacyIdChars]], true).
interpret_publication_insert(data([]), Hash, duplicate(Hash)) :-
    writeln([publication_duplicate_skipped|[Hash]], true).
interpret_publication_insert(error(Err), _, _) :-
    throw(pg_error(Err)).

interpret_publication_insert_psql(no_records_found, Hash, duplicate(Hash)) :-
    writeln([publication_duplicate_skipped|[Hash]], true).
interpret_publication_insert_psql(LegacyId, _Hash, new(LegacyIdChars)) :-
    integer(LegacyId),
    number_chars(LegacyId, LegacyIdChars),
    writeln([publication_inserted|[LegacyIdChars]], true).
interpret_publication_insert_psql([C|Cs], _Hash, new([C|Cs])) :-
    \+ integer([C|Cs]),
    dif([C|Cs], no_records_found),
    writeln([publication_inserted|[[C|Cs]]], true).
