:- module(repository_publisher_queries, [
    all_clauses/3,
    by_criteria_sql/1,
    count_matching_records_sql/1,
    count_sql/1,
    insert_sql/1,
    psql_all_clauses_sql/2,
    psql_by_criteria_sql/3,
    psql_count_matching_records_sql/3,
    psql_insert_sql/6,
    psql_select_by_did_and_name_sql/3,
    query_max_id_sql/1,
    select_by_did_and_name_sql/1
]).

:- use_module(library(lists)).
:- use_module(library(si)).

/**
The statements `repository_publisher` sends, kept apart from the sending.

Each predicate here relates a statement to the chars it is made of, by
append/2 over literals: no session, no connection, nothing that waits on
Postgres. That is the whole reason they sit apart -- a prover loads this
file on its own, whereas `repository_publisher` cannot be loaded without the
wire client, and the client cannot be loaded without sockets, so the shape
of a statement stayed out of LPTP's reach for a reason that was never its
own.

The table name sits here, beside the statements that spell it, and is not
exported: a name read across a module boundary looks like state when it is
nothing but a word in the SQL. Should `repository_publisher` come to need it
again, saying the line twice is the cheaper mistake.
*/

%% table(-Table)
table("publishers_list").

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
            "r.name as string__name, ",
            "r.screen_name as string__screen_name, ",
            "r.list_id as string__list_id, ",
            "r.public_id as string__public_id, ",
            "r.total_members as number__total_members, ",
            "r.total_statuses as number__total_statuses ",
            "FROM public.", Table, " t ",
            "ORDER BY t.id DESC, t.name ASC, t.screen_name ASC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        SQL
    ).

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
            "r.name as string__name, ",
            "r.screen_name as string__screen_name, ",
            "r.list_id as string__list_id, ",
            "r.public_id as string__public_id, ",
            "r.total_members as number__total_members, ",
            "r.total_statuses as number__total_statuses ",
            "FROM public.", Table, " t ",
            "ORDER BY t.id DESC, t.name ASC, t.screen_name ASC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        SQL
    ).

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

%% psql_by_criteria_sql(+DID, +ListURI, -SQL).
%
% Concat form: caller-supplied DID and ListURI are inlined as
% single-quoted SQL literals. Both are controlled identifiers
% (URI / DID) so no encoding is needed; the psql `--csv`-era code
% used the same convention. The trailing semicolon is omitted
% because matching_criteria/2 appends "LIMIT 0;" then "LIMIT ALL;"
% in its two-pass header/rows shape.
%
% Columns are COALESCEd to a single space because rows//1 merges
% consecutive separators: an empty (NULL) field, e.g. a list
% without total_members, would otherwise make the row unparsable.
psql_by_criteria_sql(DID, ListURI, SQL) :-
    table(Table),
    append([
        "SELECT ",
        "COALESCE(NULLIF(r.name::text, ''), ' ') as string__name, ",
        "COALESCE(NULLIF(r.screen_name::text, ''), ' ') as string__screen_name, ",
        "COALESCE(NULLIF(r.list_id::text, ''), ' ') as string__list_id, ",
        "COALESCE(NULLIF(r.public_id::text, ''), ' ') as string__public_id, ",
        "COALESCE(NULLIF(r.total_members::text, ''), ' ') as number__total_members, ",
        "COALESCE(NULLIF(r.total_statuses::text, ''), ' ') as number__total_statuses ",
        "FROM public.", Table, " r ",
        "WHERE r.created_at::date > '2024-12-31'::date ",
        "AND r.screen_name = '", DID, "' ",
        "AND r.name = '", ListURI, "' ",
        "OFFSET 0 "
    ], SQL).

by_criteria_sql(SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.name as string__name, ",
        "r.screen_name as string__screen_name, ",
        "r.list_id as string__list_id, ",
        "r.public_id as string__public_id, ",
        "r.total_members as number__total_members, ",
        "r.total_statuses as number__total_statuses ",
        "FROM public.", Table, " r ",
        "WHERE r.created_at::date > '2024-12-31'::date ",
        "AND r.screen_name = $1 ",
        "AND r.name = $2 ",
        "OFFSET 0;"
    ], SQL).

%% psql_insert_sql(+NextIdChars, +ListIdChars, +PublicId, +ListURI,
%%                  +DID, -SQL).
%
% Concatenated INSERT for the psql backend. Identifiers (DID,
% ListURI, PublicId, NextIdChars, ListIdChars) are inlined as
% single-quoted SQL literals - same convention as the pre-69f3f97
% client. No RETURNING; the caller treats success/failure via
% query_result/2's ok | no_records_found contract.
psql_insert_sql(NextIdChars, ListIdChars, PublicId, ListURI, DID, SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "id, list_id, public_id, name, screen_name, locked, created_at",
            ") VALUES (",
            "'", NextIdChars, "', ",
            " ", ListIdChars, ", ",
            "'", PublicId, "', ",
            "'", ListURI, "', ",
            "'", DID, "', ",
            "false, NOW()",
            ");"
        ],
        SQL
    ).

psql_select_by_did_and_name_sql(DID, ListURI, SQL) :-
    table(Table),
    append(
        [
            "SELECT r.id AS number__id ",
            "FROM public.", Table, " r ",
            "WHERE r.screen_name = '", DID, "' ",
            "AND r.name = '", ListURI, "';"
        ],
        SQL
    ).

insert_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "id, list_id, public_id, name, screen_name, locked, created_at",
            ") VALUES ($1::bigint, $2::bigint, $3, $4, $5, false, NOW())"
        ],
        SQL
    ).

select_by_did_and_name_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT r.id AS number__id ",
            "FROM public.", Table, " r ",
            "WHERE r.screen_name = $1 ",
            "AND r.name = $2;"
        ],
        SQL
    ).

psql_count_matching_records_sql(ListURI, DID, SQL) :-
    table(Table),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", Table, " r ",
        "WHERE name = '", ListURI, "' ",
        "AND screen_name = '", DID, "';"
    ], SQL).

count_matching_records_sql(SQL) :-
    table(Table),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", Table, " r ",
        "WHERE name = $1 ",
        "AND screen_name = $2;"
    ], SQL).
