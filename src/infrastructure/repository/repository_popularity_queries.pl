:- module(repository_popularity_queries, [
    all_clauses/3,
    by_criteria_sql/1,
    count_matching_records_sql/1,
    count_sql/1,
    insert_sql/1,
    popularity_insert_sql/1,
    psql_all_clauses_sql/2,
    psql_by_criteria_sql/2,
    psql_count_matching_records_sql/2,
    psql_insert_sql/6,
    psql_popularity_insert_sql/6,
    psql_select_by_uri_sql/2,
    query_max_id_sql/1,
    select_by_uri_sql/1
]).

:- use_module(library(lists)).
:- use_module(library(si)).

/**
The statements `repository_popularity` sends, kept apart from the sending.

Each predicate here relates a statement to the chars it is made of, by
append/2 over literals: no session, no connection, nothing that waits on
Postgres. That is the whole reason they sit apart -- a prover loads this
file on its own, whereas `repository_popularity` cannot be loaded without
the wire client, and the client cannot be loaded without sockets, so the
shape of a statement stayed out of LPTP's reach for a reason that was never
its own.

The table name sits here, beside the statements that spell it, and is not
exported: a name read across a module boundary looks like state when it is
nothing but a word in the SQL. Should `repository_popularity` come to need
it again, saying the line twice is the cheaper mistake.
*/

table("status_popularity").

count_sql(SQL) :-
    table(Table),
    append(["SELECT COUNT(*) AS matching_records_count FROM public.", Table], SQL).

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
