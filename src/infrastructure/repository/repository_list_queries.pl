:- module(repository_list_queries, [
    all_clauses/3,
    by_list_uri_sql/1,
    count_matching_records_sql/1,
    count_sql/1,
    insert_event_sql/1,
    psql_all_clauses_sql/2,
    psql_by_list_uri_sql/2,
    psql_count_matching_records_sql/2,
    psql_insert_event_sql/5,
    psql_select_payload_sql/2,
    query_event_max_id_sql/1,
    query_max_id_sql/1,
    select_payload_sql/1
]).

:- use_module(library(lists)).
:- use_module(library(si)).

/**
The statements `repository_list` sends, kept apart from the sending.

Each predicate here relates a statement to the chars it is made of, by
append/2 over literals: no session, no connection, nothing that waits on
Postgres. That is the whole reason they sit apart -- a prover loads this
file on its own, whereas `repository_list` cannot be loaded without the wire
client, and the client cannot be loaded without sockets, so the shape of a
statement stayed out of LPTP's reach for a reason that was never its own.

The table names sit here, beside the statements that spell them, and are not
exported: a name read across a module boundary looks like state when it is
nothing but a word in the SQL. Should `repository_list` come to need one
again, saying the line twice is the cheaper mistake.
*/

%% table(-Table)
table("publishers_list").

%% event_table(-EventTable).
event_table("publishers_list_collected_event").

count_sql(SQL) :-
    table(Table),
    append(["SELECT count(*) AS matching_records_count FROM public.", Table], SQL).

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

%% psql_by_list_uri_sql(+ListURI, -SQL).
%
% Concat form: ListURI is inlined as a single-quoted SQL literal,
% same convention as the pre-69f3f97 client. Trailing semicolon is
% omitted because matching_criteria/2 appends "LIMIT 0;" then
% "LIMIT ALL;" in its two-pass header/rows shape.
psql_by_list_uri_sql(ListURI, SQL) :-
    event_table(EventTable),
    append([
        "SELECT ",
        "payload AS string__payload, ",
        "list_id AS number__list_id, ",
        "list_name AS string__list_name ",
        "FROM public.", EventTable, " e ",
        "WHERE e.occurred_at::date > '2024-12-31'::date ",
        "AND e.list_name = '", ListURI, "' ",
        "OFFSET 0 "
    ], SQL).

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

psql_count_matching_records_sql(ListName, SQL) :-
    event_table(EventTable),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", EventTable, " ",
        "WHERE list_name = '", ListName, "';"
    ], SQL).

count_matching_records_sql(SQL) :-
    event_table(EventTable),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", EventTable, " ",
        "WHERE list_name = $1;"
    ], SQL).

%% psql_insert_event_sql(+NextPrimaryKey, +NextIdChars, +ListName,
%%                       +Payload, -SQL).
%
% Concatenated INSERT for the psql backend. Identifiers are inlined
% as single-quoted SQL literals, the same convention as the
% pre-69f3f97 client. No RETURNING; the caller treats success/failure
% via query_result/2's ok | no_records_found contract.
psql_insert_event_sql(NextPrimaryKey, NextIdChars, ListName, Payload, SQL) :-
    event_table(EventTable),
    append(
        [
            "INSERT INTO public.", EventTable, " (",
            "id, list_id, list_name, payload, occurred_at, started_at, ended_at",
            ") VALUES (",
            "'", NextPrimaryKey, "', ",
            " ", NextIdChars, ", ",
            "'", ListName, "', ",
            "'", Payload, "', ",
            "NOW(), NOW(), NOW()",
            ");"
        ],
        SQL
    ).

psql_select_payload_sql(ListName, SQL) :-
    event_table(EventTable),
    append(
        [
            "SELECT l.payload AS payload ",
            "FROM public.", EventTable, " l ",
            "WHERE l.list_name = '", ListName, "' ",
            "ORDER BY l.occurred_at DESC LIMIT 1;"
        ],
        SQL
    ).

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
            "WHERE l.list_name = $1 ",
            "ORDER BY l.occurred_at DESC LIMIT 1;"
        ],
        SQL
    ).
