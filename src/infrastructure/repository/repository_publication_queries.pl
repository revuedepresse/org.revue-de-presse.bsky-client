:- module(repository_publication_queries, [
    all_clauses/3,
    by_criteria_sql/1,
    count_sql/1,
    psql_all_clauses_sql/2,
    psql_by_criteria_sql/2,
    psql_publication_insert_sql/10,
    publication_insert_sql/1,
    query_max_id_sql/1
]).

:- use_module(library(lists)).
:- use_module(library(si)).

/**
The statements `repository_publication` sends, kept apart from the sending.

Each predicate here relates a statement to the chars it is made of, by
append/2 over literals: no session, no connection, nothing that waits on
Postgres. That is the whole reason they sit apart -- a prover loads this
file on its own, whereas `repository_publication` cannot be loaded without
the wire client, and the client cannot be loaded without sockets, so the
shape of a statement stayed out of LPTP's reach for a reason that was never
its own.

The table name sits here, beside the statements that spell it, and is not
exported: a name read across a module boundary looks like state when it is
nothing but a word in the SQL. Should `repository_publication` come to need
it again, saying the line twice is the cheaper mistake.
*/

table("publication").

count_sql(SQL) :-
    table(Table),
    append(["SELECT count(*) AS matching_records_count FROM public.", Table], SQL).

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
