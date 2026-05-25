:- module(repository_publisher, [
    by_criteria/3,
    count/1,
    insert/2,
    next_id/1,
    query/1
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
    execute/2,
    matching_criteria/2,
    matching_criteria/4,
    query_result/2,
    query_result_from_file/3,
    query_result_from_file/4,
    read_rows/2,
    value/3
]).
:- use_module('../../configuration', [pg_backend/1]).
:- use_module('../../logger', [
    log_debug/1,
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
Repository for individual `publishers_list` rows.

Each row binds a DID to a list (via `list_id`) and carries
the human-readable name / handle. Reads scope on DID and list
URI; writes go through a pre-count + insert pattern so
duplicates are reported rather than written twice.
*/

%% table(-Table)
table("publishers_list").

%% count(-Count)
%
% Total number of rows in `publishers_list`. Dispatches on PG_BACKEND.
count(Count) :-
    pg_backend("wire"),
    count_sql(SQL),
    value(SQL, [], Count).
count(Count) :-
    pg_backend("psql"),
    count_sql(SQL),
    query_result(SQL, Count).

count_sql(SQL) :-
    table(Table),
    append(["SELECT count(*) AS matching_records_count FROM public.", Table], SQL).

%% query(-HeadersAndRows)
%
% All `publishers_list` rows as header-keyed assocs.
query(HeadersAndRows) :-
    listing_headers(Headers),
    query(Rows, [], "ALL"),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

listing_headers([
    "string__name",
    "string__screen_name",
    "string__list_id",
    "string__public_id",
    "number__total_members",
    "number__total_statuses"
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

%% query(-Rows, +Headers, +Limit).
query(Rows, Headers, Limit) :-
    pg_backend("wire"),
    once(all_clauses(SQL, Params, Limit)),
    once(query_result_from_file(SQL, Params, Headers, TmpFile)),
    read_rows(TmpFile, Rows).
query(Rows, _Headers, Limit) :-
    pg_backend("psql"),
    psql_all_clauses_sql(SQL, Limit),
    once(query_result_from_file(SQL, true, TmpFile)),
    read_rows(TmpFile, Rows).

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

%% next_id(-NextId)
%
% Next available `id` for `publishers_list`. Dispatches on PG_BACKEND.
next_id(NextId) :-
    pg_backend("wire"),
    query_max_id_sql(SQL),
    value(SQL, [], MaxIdValue),
    coerce_no_records(MaxIdValue, MaxId),
    NextId #= MaxId + 1,
    validate_id(NextId).
next_id(NextId) :-
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
            "SELECT COALESCE(r.id::bigint, 0) pk ",
            "FROM ", Table, " r ",
            "ORDER BY r.id DESC ",
            "LIMIT 1;"
        ],
        SQL
    ).

%% by_criteria(+ListURI, +DID, -HeadersAndRows)
%
% Look up `publishers_list` rows for the given list URI and
% DID combination as header-keyed assocs. Dispatches on PG_BACKEND.
by_criteria(list_uri(ListURI), did(DID), HeadersAndRows) :-
    pg_backend("wire"),
    chars_si(DID),
    by_criteria_headers(Headers),
    by_criteria_sql(SQL),
    matching_criteria(SQL, [DID, ListURI], Headers, HeadersAndRows).
by_criteria(list_uri(ListURI), did(DID), HeadersAndRows) :-
    pg_backend("psql"),
    chars_si(DID),
    psql_by_criteria_sql(DID, ListURI, SQL),
    matching_criteria(SQL, HeadersAndRows).

%% psql_by_criteria_sql(+DID, +ListURI, -SQL).
%
% Concat form: caller-supplied DID and ListURI are inlined as
% single-quoted SQL literals. Both are controlled identifiers
% (URI / DID) so no encoding is needed; the psql `--csv`-era code
% used the same convention. The trailing semicolon is omitted
% because matching_criteria/2 appends "LIMIT 0;" then "LIMIT ALL;"
% in its two-pass header/rows shape.
psql_by_criteria_sql(DID, ListURI, SQL) :-
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
        "AND r.screen_name = '", DID, "' ",
        "AND r.name = '", ListURI, "' ",
        "OFFSET 0 "
    ], SQL).

by_criteria_headers([
    "string__name",
    "string__screen_name",
    "string__list_id",
    "string__public_id",
    "number__total_members",
    "number__total_statuses"
]).

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

%% insert(+Row, -InsertionResult)
%
% Insert a new `publishers_list` row, or report the existing
% row id if `(ListURI, DID)` already exists. Always succeeds
% with `ok`.
insert(row(ListId, ListURI, _FollowersCount, _FollowsCount, DID, _), InsertionResult) :-
    count_matching_records(row(ListURI, DID), TotalMatchingRecords),
    if_(
        dif(1, TotalMatchingRecords),
        insert_new_publisher(ListId, ListURI, DID, InsertionResult),
        report_existing_publisher(ListURI, DID, InsertionResult)
    ).

insert_new_publisher(ListId, ListURI, DID, ok) :-
    pg_backend("wire"),
    next_id(NextId),
    number_chars(NextId, NextIdChars),
    uuidv4_string(PublicId),
    coerce_chars(ListId, ListIdChars),
    insert_sql(SQL),
    execute(SQL, [NextIdChars, ListIdChars, PublicId, ListURI, DID]).
insert_new_publisher(ListId, ListURI, DID, ok) :-
    pg_backend("psql"),
    next_id(NextId),
    number_chars(NextId, NextIdChars),
    uuidv4_string(PublicId),
    coerce_chars(ListId, ListIdChars),
    psql_insert_sql(NextIdChars, ListIdChars, PublicId, ListURI, DID, SQL),
    query_result(SQL, _Result).

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

report_existing_publisher(ListURI, DID, ok) :-
    pg_backend("wire"),
    select_by_did_and_name_sql(SelectSQL),
    value(SelectSQL, [DID, ListURI], ExistingId),
    log_existing_publisher(ExistingId).
report_existing_publisher(ListURI, DID, ok) :-
    pg_backend("psql"),
    psql_select_by_did_and_name_sql(DID, ListURI, SelectSQL),
    query_result(SelectSQL, ExistingId),
    log_existing_publisher(ExistingId).

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

log_existing_publisher(no_records_found) :- log_info([no_publisher_record_found]).
log_existing_publisher(ExistingId) :-
    dif(ExistingId, no_records_found),
    log_info([publisher_already_exists(ExistingId)]).

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

coerce_chars(N, Chars) :- integer(N), number_chars(N, Chars).
coerce_chars(A, Chars) :- \+ integer(A), atom(A), atom_chars(A, Chars).
coerce_chars(V, V) :- \+ integer(V), \+ atom(V).

%% count_matching_records(+Row, -Result). Dispatches on PG_BACKEND.
count_matching_records(row(ListURI, DID), Result) :-
    pg_backend("wire"),
    count_matching_records_sql(SQL),
    value(SQL, [ListURI, DID], Result).
count_matching_records(row(ListURI, DID), Result) :-
    pg_backend("psql"),
    psql_count_matching_records_sql(ListURI, DID, SQL),
    query_result(SQL, Result).

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
