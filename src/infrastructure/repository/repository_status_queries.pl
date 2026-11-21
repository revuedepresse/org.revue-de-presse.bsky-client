:- module(repository_status_queries, [
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

:- use_module(library(lists)).
:- use_module(library(si)).

/**
The statements `repository_status` sends, kept apart from the sending.

Each predicate here relates a statement to the chars it is made of, by
append/2 over literals: no session, no connection, nothing that waits on
Postgres. That is the whole reason they sit apart -- a prover loads this
file on its own, whereas `repository_status` cannot be loaded without the
wire client, and the client cannot be loaded without sockets, so the shape
of a statement stayed out of LPTP's reach for a reason that was never its
own.

The table name sits here, beside the statements that spell it, and is not
exported: a name read across a module boundary looks like state when it is
nothing but a word in the SQL. Should `repository_status` come to need it
again, saying the line twice is the cheaper mistake.
*/

%% table(-Table)
table("weaving_status").

count_sql(SQL) :-
    table(Table),
    append(["SELECT count(*) AS matching_records_count FROM public.", Table], SQL).

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

status_lookup_by_hash_sql(SQL) :-
    table(Table),
    append(["SELECT ust_id::text FROM public.", Table, " WHERE ust_hash = $1"], SQL).

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

count_matching_records_sql(SQL) :-
    table(Table),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", Table, " r ",
        "WHERE ust_hash = $1;"
    ], SQL).

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
