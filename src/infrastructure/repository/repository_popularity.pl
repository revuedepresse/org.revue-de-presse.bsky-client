:- module(repository_popularity, [
    by_criteria/3,
    count/2,
    insert/3,
    insert_without_unicity_check/3,
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
    execute/3,
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
Repository for per-post popularity counters. Every wire-path
predicate takes a `pg_session(In, Out)` compound as its first
argument; psql-path clauses thread the session as a passthrough.
*/

table("status_popularity").

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
    append(["SELECT COUNT(*) AS matching_records_count FROM public.", Table], SQL).

query(Session, HeadersAndRows) :-
    listing_headers(Headers),
    query(Session, Rows, [], 2),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

listing_headers([
    "string__id",
    "string__id",
    "number__likes",
    "number__reposts",
    "number__string"
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
            "r.id as string__id, ",
            "r.publication_id as string__id, ",
            "r.total_favorites as number__likes, ",
            "r.total_retweets as number__reposts, ",
            "r.checked_at as number__string ",
            "FROM public.", Table, " r ",
            "ORDER BY status_id DESC ",
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
            "r.id as string__id, ",
            "r.publication_id as string__id, ",
            "r.total_favorites as number__likes, ",
            "r.total_retweets as number__reposts, ",
            "r.checked_at as number__string ",
            "FROM public.", Table, " r ",
            "ORDER BY status_id DESC ",
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
            "SELECT COALESCE(r.status_id::bigint, 0) pk ",
            "FROM ", Table, " r ",
            "ORDER BY r.status_id DESC ",
            "LIMIT 1;"
        ],
        SQL
    ).

by_criteria(pg_session(C0, C), record_id(RecordId), HeadersAndRows) :-
    pg_backend("wire"),
    chars_si(RecordId),
    by_criteria_headers(Headers),
    by_criteria_sql(SQL),
    matching_criteria(pg_session(C0, C), SQL, [RecordId], Headers, HeadersAndRows).
by_criteria(pg_session(C, C), record_id(RecordId), HeadersAndRows) :-
    pg_backend("psql"),
    chars_si(RecordId),
    psql_by_criteria_sql(RecordId, SQL),
    matching_criteria(SQL, HeadersAndRows).

psql_by_criteria_sql(RecordId, SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.id as string__id, ",
        "r.publication_id as string__id, ",
        "r.total_favorites as number__likes, ",
        "r.total_retweets as number__reposts, ",
        "r.checked_at as number__string ",
        "FROM public.", Table, " r ",
        "WHERE r.publication_id = '", RecordId, "' ",
        "OFFSET 0 "
    ], SQL).

by_criteria_headers([
    "string__id",
    "string__id",
    "number__likes",
    "number__reposts",
    "number__string"
]).

by_criteria_sql(SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.id as string__id, ",
        "r.publication_id as string__id, ",
        "r.total_favorites as number__likes, ",
        "r.total_retweets as number__reposts, ",
        "r.checked_at as number__string ",
        "FROM public.", Table, " r ",
        "WHERE r.publication_id = $1 ",
        "OFFSET 0;"
    ], SQL).

%% insert(+Session, +Row, -InsertionResult)
insert(
    pg_session(C0, C),
    row(RecordId, URI, Likes, RePosts),
    InsertionResult
) :-
    count_matching_records(pg_session(C0, C1), row(URI), TotalMatchingRecords),
    if_(
        dif(1, TotalMatchingRecords),
        insert_new_popularity(pg_session(C1, C), RecordId, URI, Likes, RePosts, InsertionResult),
        report_existing_popularity(pg_session(C1, C), URI, InsertionResult)
    ).

insert_new_popularity(pg_session(C0, C), RecordId, URI, Likes, RePosts, ok) :-
    pg_backend("wire"),
    uuidv4_string(Id),
    coerce_chars(Likes, LikesChars),
    coerce_chars(RePosts, RePostsChars),
    coerce_chars(RecordId, RecordIdChars),
    insert_sql(SQL),
    execute(pg_session(C0, C), SQL, [Id, RecordIdChars, URI, LikesChars, RePostsChars]).
insert_new_popularity(pg_session(C, C), RecordId, URI, Likes, RePosts, ok) :-
    pg_backend("psql"),
    uuidv4_string(Id),
    coerce_chars(Likes, LikesChars),
    coerce_chars(RePosts, RePostsChars),
    coerce_chars(RecordId, RecordIdChars),
    psql_insert_sql(Id, RecordIdChars, URI, LikesChars, RePostsChars, SQL),
    query_result(SQL, _Result).

psql_insert_sql(Id, RecordIdChars, URI, LikesChars, RePostsChars, SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "id, status_id, publication_id, total_favorites, total_retweets, checked_at",
            ") VALUES (",
            "'", Id, "', ",
            "'", RecordIdChars, "', ",
            "'", URI, "', ",
            LikesChars, ", ",
            RePostsChars, ", ",
            "NOW()",
            ");"
        ],
        SQL
    ).

report_existing_popularity(pg_session(C0, C), URI, ok) :-
    pg_backend("wire"),
    select_by_uri_headers(SelectHeaders),
    select_by_uri_sql(SelectSQL),
    matching_criteria(pg_session(C0, C), SelectSQL, [URI], SelectHeaders, Rows),
    writeln([popularity_rows|[Rows]], true).
report_existing_popularity(pg_session(C, C), URI, ok) :-
    pg_backend("psql"),
    psql_select_by_uri_sql(URI, SelectSQL),
    matching_criteria(SelectSQL, Rows),
    writeln([popularity_rows|[Rows]], true).

psql_select_by_uri_sql(URI, SQL) :-
    table(Table),
    append(
        [
            "SELECT ",
            "r.publication_id as string__id, ",
            "r.total_favorites as number__likes, ",
            "r.total_retweets as number__reposts, ",
            "r.checked_at as number__string ",
            "FROM public.", Table, " r ",
            "WHERE r.publication_id = '", URI, "' "
        ],
        SQL
    ).

insert_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "id, status_id, publication_id, total_favorites, total_retweets, checked_at",
            ") VALUES ($1, $2, $3, $4::integer, $5::integer, NOW());"
        ],
        SQL
    ).

select_by_uri_headers([
    "string__id",
    "number__likes",
    "number__reposts",
    "number__string"
]).

select_by_uri_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT ",
            "r.publication_id as string__id, ",
            "r.total_favorites as number__likes, ",
            "r.total_retweets as number__reposts, ",
            "r.checked_at as number__string ",
            "FROM public.", Table, " r ",
            "WHERE r.publication_id = $1;"
        ],
        SQL
    ).

%% insert_without_unicity_check(+Session, +Row, -InsertionResult)
%
% Hot-path entry point used by event_getAuthorFeed. Append-only;
% no pre-count. Threads the session through so a wire reset gets
% picked up by the popularity insert as well.
insert_without_unicity_check(Session, Row, InsertionResult) :-
    once(insert_without_unicity_check_attempt(Session, Row, InsertionResult)).

insert_without_unicity_check_attempt(Session, Row, InsertionResult) :-
    catch(
        do_insert_without_unicity_check(Session, Row, InsertionResult),
        Cause,
        log_error([cannot_insert_popularity_without_unicity_check(Cause)])
    ),
    writeln([inserted_popularity_without_unicity_check_successfully|[]], true).
insert_without_unicity_check_attempt(pg_session(C, C), Row, _) :-
    writeln([cannot_insert_popularity_without_unicity_check(Row)], true).

do_insert_without_unicity_check(
    pg_session(C0, C),
    row(RecordId, URI, Likes, RePosts),
    InsertionResult
) :-
    pg_backend("wire"),
    uuidv4_string(Id),
    coerce_chars(Likes, LikesChars),
    coerce_chars(RePosts, RePostsChars),
    coerce_chars(RecordId, RecordIdChars),
    popularity_insert_sql(SQL),
    pg_query_or_throw(pg_session(C0, C), SQL, [Id, RecordIdChars, URI, LikesChars, RePostsChars], Reply),
    interpret_popularity_insert(Reply, InsertionResult).
do_insert_without_unicity_check(
    pg_session(C, C),
    row(RecordId, URI, Likes, RePosts),
    InsertionResult
) :-
    pg_backend("psql"),
    uuidv4_string(Id),
    coerce_chars(Likes, LikesChars),
    coerce_chars(RePosts, RePostsChars),
    coerce_chars(RecordId, RecordIdChars),
    psql_popularity_insert_sql(Id, RecordIdChars, URI, LikesChars, RePostsChars, SQL),
    query_result(SQL, Result),
    interpret_popularity_insert_psql(Result, InsertionResult).

psql_popularity_insert_sql(Id, RecordIdChars, URI, LikesChars, RePostsChars, SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "id, status_id, publication_id, total_favorites, total_retweets, checked_at",
            ") VALUES (",
            "'", Id, "', ",
            "'", RecordIdChars, "', ",
            "'", URI, "', ",
            LikesChars, ", ",
            RePostsChars, ", ",
            "NOW()",
            ");"
        ],
        SQL
    ).

interpret_popularity_insert_psql(ok, ok).
interpret_popularity_insert_psql(no_records_found, ok).
interpret_popularity_insert_psql(Other, _) :-
    dif(Other, ok),
    dif(Other, no_records_found),
    throw(pg_error(Other)).

popularity_insert_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "id, status_id, publication_id, total_favorites, total_retweets, checked_at",
            ") VALUES ($1, $2, $3, $4::integer, $5::integer, NOW())"
        ],
        SQL
    ).

coerce_chars(N, Chars) :- integer(N), number_chars(N, Chars).
coerce_chars(A, Chars) :- \+ integer(A), atom(A), atom_chars(A, Chars).
coerce_chars(V, V) :- \+ integer(V), \+ atom(V).

interpret_popularity_insert(data(_), ok).
interpret_popularity_insert(error(Err), _) :- throw(pg_error(Err)).

count_matching_records(pg_session(C0, C), row(URI), Result) :-
    pg_backend("wire"),
    count_matching_records_sql(SQL),
    value(pg_session(C0, C), SQL, [URI], Result).
count_matching_records(pg_session(C, C), row(URI), Result) :-
    pg_backend("psql"),
    psql_count_matching_records_sql(URI, SQL),
    query_result(SQL, Result).

count_matching_records_sql(SQL) :-
    table(Table),
    append([
        "SELECT COUNT(*) how_many_records ",
        "FROM public.", Table, " r ",
        "WHERE r.publication_id = $1;"
    ], SQL).

psql_count_matching_records_sql(URI, SQL) :-
    table(Table),
    append([
        "SELECT COUNT(*) how_many_records ",
        "FROM public.", Table, " r ",
        "WHERE r.publication_id = '", URI, "';"
    ], SQL).
