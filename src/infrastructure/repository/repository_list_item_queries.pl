:- module(repository_list_item_queries, [
    all_clauses/3,
    count_matching_events_sql/1,
    count_matching_records_sql/1,
    count_sql/1,
    event_by_screen_name_sql/1,
    insert_event_sql/1,
    insert_record_sql/1,
    psql_all_clauses_sql/2,
    psql_count_matching_events_sql/2,
    psql_count_matching_records_sql/2,
    psql_event_by_screen_name_sql/2,
    psql_insert_event_sql/4,
    psql_insert_record_sql/7,
    psql_record_by_did_sql/2,
    psql_record_by_screen_name_sql/2,
    psql_select_event_payload_sql/2,
    psql_select_record_id_sql/2,
    psql_update_record_handle_sql/3,
    query_max_id_sql/1,
    record_by_did_sql/1,
    record_by_screen_name_sql/1,
    select_event_payload_sql/1,
    select_record_id_sql/1,
    update_record_handle_sql/1
]).

:- use_module(library(lists)).
:- use_module(library(si)).

/**
The statements `repository_list_item` sends, kept apart from the sending.

Each predicate here relates a statement to the chars it is made of, by
append/2 over literals: no session, no connection, nothing that waits on
Postgres. That is the whole reason they sit apart -- a prover loads this
file on its own, whereas `repository_list_item` cannot be loaded without the
wire client, and the client cannot be loaded without sockets, so the shape
of a statement stayed out of LPTP's reach for a reason that was never its
own.

The table names sit here, beside the statements that spell them, and are not
exported: a name read across a module boundary looks like state when it is
nothing but a word in the SQL. Should `repository_list_item` come to need
one again, saying the line twice is the cheaper mistake.
*/

%% table(-Table).
table("weaving_user").

%% event_table(-EventTable).
event_table("member_profile_collected_event").

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
            "r.usr_id::bigint AS number__id, ",
            "r.usr_twitter_username::text AS string__handle, ",
            "r.usr_twitter_id::text AS string__did, ",
            "COALESCE(r.usr_user_name::text, r.usr_twitter_username::text, ' ') AS string__username, ",
            "COALESCE(NULLIF(r.usr_avatar::text, ''), ' ') AS string__avatar, ",
            "r.description::bytea AS string__description, ",
            "COALESCE(NULLIF(r.url::text, ''), ' ') AS string__uri, ",
            "COALESCE(NULLIF(r.last_status_publication_date::text, ''), ' ') AS string__indexed_at ",
            "FROM public.", Table, " r ",
            "ORDER BY r.usr_id DESC ",
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
            "r.usr_id::bigint AS number__id, ",
            "r.usr_twitter_username::text AS string__handle, ",
            "r.usr_twitter_id::text AS string__did, ",
            "COALESCE(r.usr_user_name::text, r.usr_twitter_username::text, ' ') AS string__username, ",
            "COALESCE(NULLIF(r.usr_avatar::text, ''), ' ') AS string__avatar, ",
            "r.description::bytea AS string__description, ",
            "COALESCE(NULLIF(r.url::text, ''), ' ') AS string__uri, ",
            "COALESCE(NULLIF(r.last_status_publication_date::text, ''), ' ') AS string__indexed_at ",
            "FROM public.", Table, " r ",
            "ORDER BY r.usr_id DESC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        SQL
    ).

query_max_id_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT u.usr_id pk ",
            "FROM ", Table, " u ",
            "ORDER BY pk DESC ",
            "LIMIT 1;"
        ],
        SQL
    ).

%% psql_event_by_screen_name_sql(+ScreenName, -SQL).
%
% Concat form of the event_by_screen_name SELECT for the psql
% backend. ScreenName is inlined as a single-quoted SQL literal
% (controlled identifier, same convention as the pre-69f3f97 client).
psql_event_by_screen_name_sql(ScreenName, SQL) :-
    event_table(EventTable),
    append([
        "SELECT payload AS string__payload, screen_name AS string__screen_name ",
        "FROM public.", EventTable, " e ",
        "WHERE e.occurred_at::date > '2024-12-31'::date ",
        "AND e.screen_name = '", ScreenName, "' ",
        "OFFSET 0 LIMIT ALL;"
    ], SQL).

event_by_screen_name_sql(SQL) :-
    event_table(EventTable),
    append([
        "SELECT payload AS string__payload, screen_name AS string__screen_name ",
        "FROM public.", EventTable, " e ",
        "WHERE e.occurred_at::date > '2024-12-31'::date ",
        "AND e.screen_name = $1 ",
        "OFFSET 0 LIMIT ALL;"
    ], SQL).

%% psql_record_by_screen_name_sql(+ScreenName, -SQL).
%
% Concat form of the record_by_screen_name SELECT for the psql
% backend. ScreenName is inlined as a single-quoted SQL literal
% (controlled identifier, same convention as the pre-69f3f97 client).
psql_record_by_screen_name_sql(ScreenName, SQL) :-
    table(Table),
    append([
        "SELECT ",
        "COALESCE(NULLIF(r.usr_id::text, ''), ' ') AS number__id, ",
        "COALESCE(NULLIF(r.usr_api_key::text, ''), ' ') AS string__api_key, ",
        "COALESCE(NULLIF(r.max_status_id::text, ''), ' ') AS number__max_status_id, ",
        "COALESCE(NULLIF(r.min_status_id::text, ''), ' ') AS number__min_status_id, ",
        "COALESCE(NULLIF(r.max_like_id::text, ''), ' ') AS number__max_like_id, ",
        "COALESCE(NULLIF(r.min_like_id::text, ''), ' ') AS number__min_like_id, ",
        "COALESCE(NULLIF(r.total_statuses::text, ''), ' ') AS number__total_statuses, ",
        "COALESCE(NULLIF(r.total_likes::text, ''), ' ') AS number__total_likes, ",
        "COALESCE(NULLIF(r.description::text, ''), ' ') AS string__description, ",
        "COALESCE(NULLIF(r.url::text, ''), ' ') AS string__url, ",
        "COALESCE(NULLIF(r.last_status_publication_date::text, ''), ' ') AS string__last_status_publication_date, ",
        "COALESCE(NULLIF(r.total_subscribees::text, ''), ' ') AS number__total_subscribees, ",
        "COALESCE(NULLIF(r.total_subscriptions::text, ''), ' ') AS number__total_subscriptions, ",
        "COALESCE(NULLIF(r.usr_twitter_id::text, ''), ' ') AS string__twitter_id, ",
        "COALESCE(NULLIF(r.usr_twitter_username::text, ''), ' ') AS string__twitter_username, ",
        "COALESCE(NULLIF(r.usr_avatar::text, ''), ' ') AS string__avatar, ",
        "COALESCE(NULLIF(r.usr_full_name::text, ''), ' ') AS string__full_name, ",
        "COALESCE(NULLIF(r.usr_status::text, ''), ' ') AS boolean__status, ",
        "COALESCE(NULLIF(r.usr_user_name::text, ''), ' ') AS string__user_name, ",
        "COALESCE(NULLIF(r.usr_username_canonical::text, ''), ' ') AS string__user_name_canonical, ",
        "COALESCE(NULLIF(r.usr_email::text, ''), ' ') AS string__email, ",
        "COALESCE(NULLIF(r.usr_email_canonical::text, ''), ' ') AS string__email_canonical, ",
        "COALESCE(NULLIF(r.protected::text, ''), ' ') AS boolean__protected, ",
        "COALESCE(NULLIF(r.suspended::text, ''), ' ') AS boolean__suspended, ",
        "COALESCE(NULLIF(r.not_found::text, ''), ' ') AS boolean__not_found, ",
        "COALESCE(NULLIF(r.usr_position_in_hierarchy::text, ''), ' ') AS number__position_in_hierarchy ",
        "FROM public.", Table, " r ",
        "WHERE r.usr_twitter_username = '", ScreenName, "' ",
        "OFFSET 0 LIMIT ALL;"
    ], SQL).

% Every column is COALESCEd to a single space: rows//1 merges
% consecutive separators, so an empty (NULL or '') field would shift
% the columns after it and make to_json/3 fail. Same convention
% as all_clauses_template/2.
record_by_screen_name_sql(SQL) :-
    table(Table),
    append([
        "SELECT ",
        "COALESCE(NULLIF(r.usr_id::text, ''), ' ') AS number__id, ",
        "COALESCE(NULLIF(r.usr_api_key::text, ''), ' ') AS string__api_key, ",
        "COALESCE(NULLIF(r.max_status_id::text, ''), ' ') AS number__max_status_id, ",
        "COALESCE(NULLIF(r.min_status_id::text, ''), ' ') AS number__min_status_id, ",
        "COALESCE(NULLIF(r.max_like_id::text, ''), ' ') AS number__max_like_id, ",
        "COALESCE(NULLIF(r.min_like_id::text, ''), ' ') AS number__min_like_id, ",
        "COALESCE(NULLIF(r.total_statuses::text, ''), ' ') AS number__total_statuses, ",
        "COALESCE(NULLIF(r.total_likes::text, ''), ' ') AS number__total_likes, ",
        "COALESCE(NULLIF(r.description::text, ''), ' ') AS string__description, ",
        "COALESCE(NULLIF(r.url::text, ''), ' ') AS string__url, ",
        "COALESCE(NULLIF(r.last_status_publication_date::text, ''), ' ') AS string__last_status_publication_date, ",
        "COALESCE(NULLIF(r.total_subscribees::text, ''), ' ') AS number__total_subscribees, ",
        "COALESCE(NULLIF(r.total_subscriptions::text, ''), ' ') AS number__total_subscriptions, ",
        "COALESCE(NULLIF(r.usr_twitter_id::text, ''), ' ') AS string__twitter_id, ",
        "COALESCE(NULLIF(r.usr_twitter_username::text, ''), ' ') AS string__twitter_username, ",
        "COALESCE(NULLIF(r.usr_avatar::text, ''), ' ') AS string__avatar, ",
        "COALESCE(NULLIF(r.usr_full_name::text, ''), ' ') AS string__full_name, ",
        "COALESCE(NULLIF(r.usr_status::text, ''), ' ') AS boolean__status, ",
        "COALESCE(NULLIF(r.usr_user_name::text, ''), ' ') AS string__user_name, ",
        "COALESCE(NULLIF(r.usr_username_canonical::text, ''), ' ') AS string__user_name_canonical, ",
        "COALESCE(NULLIF(r.usr_email::text, ''), ' ') AS string__email, ",
        "COALESCE(NULLIF(r.usr_email_canonical::text, ''), ' ') AS string__email_canonical, ",
        "COALESCE(NULLIF(r.protected::text, ''), ' ') AS boolean__protected, ",
        "COALESCE(NULLIF(r.suspended::text, ''), ' ') AS boolean__suspended, ",
        "COALESCE(NULLIF(r.not_found::text, ''), ' ') AS boolean__not_found, ",
        "COALESCE(NULLIF(r.usr_position_in_hierarchy::text, ''), ' ') AS number__position_in_hierarchy ",
        "FROM public.", Table, " r ",
        "WHERE r.usr_twitter_username = $1 ",
        "OFFSET 0 LIMIT ALL;"
    ], SQL).

record_by_did_sql(SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.usr_id::text AS string__id, ",
        "COALESCE(NULLIF(r.usr_twitter_username::text, ''), ' ') AS string__twitter_username ",
        "FROM public.", Table, " r ",
        "WHERE r.usr_twitter_id = $1 ",
        "ORDER BY r.usr_id DESC ",
        "OFFSET 0 LIMIT ALL;"
    ], SQL).

%% psql_record_by_did_sql(+Did, -SQL).
%
% Concat form of the record_by_did SELECT for the psql backend. Did
% is inlined as a single-quoted SQL literal (controlled identifier,
% same convention as the pre-69f3f97 client).
psql_record_by_did_sql(Did, SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.usr_id::text AS string__id, ",
        "COALESCE(NULLIF(r.usr_twitter_username::text, ''), ' ') AS string__twitter_username ",
        "FROM public.", Table, " r ",
        "WHERE r.usr_twitter_id = '", Did, "' ",
        "ORDER BY r.usr_id DESC ",
        "OFFSET 0 LIMIT ALL;"
    ], SQL).

% SET expressions read the row as it was before the UPDATE, so each
% CASE compares against the previous handle.
update_record_handle_sql(SQL) :-
    table(Table),
    append([
        "UPDATE public.", Table, " SET ",
        "usr_twitter_username = $1::text, ",
        "usr_user_name = CASE WHEN usr_user_name = usr_twitter_username ",
        "THEN $1::text ELSE usr_user_name END, ",
        "usr_username_canonical = CASE WHEN usr_username_canonical = usr_twitter_username ",
        "THEN $1::text ELSE usr_username_canonical END, ",
        "url = CASE WHEN url = usr_twitter_username ",
        "THEN $1::text ELSE url END ",
        "WHERE usr_id = $2::bigint"
    ], SQL).

%% psql_update_record_handle_sql(+Id, +Handle, -SQL).
%
% Concat form of the handle UPDATE for the psql backend. Id and
% Handle are inlined as single-quoted SQL literals (controlled
% identifiers, same convention as the pre-69f3f97 client).
psql_update_record_handle_sql(Id, Handle, SQL) :-
    table(Table),
    append([
        "UPDATE public.", Table, " SET ",
        "usr_twitter_username = '", Handle, "', ",
        "usr_user_name = CASE WHEN usr_user_name = usr_twitter_username ",
        "THEN '", Handle, "' ELSE usr_user_name END, ",
        "usr_username_canonical = CASE WHEN usr_username_canonical = usr_twitter_username ",
        "THEN '", Handle, "' ELSE usr_username_canonical END, ",
        "url = CASE WHEN url = usr_twitter_username ",
        "THEN '", Handle, "' ELSE url END ",
        "WHERE usr_id = '", Id, "'::bigint;"
    ], SQL).

%% psql_insert_event_sql(+NextId, +ScreenName, +Payload, -SQL).
%
% Concat form of the event INSERT for the psql backend. Identifiers
% (NextId, ScreenName, Payload) are inlined as single-quoted SQL
% literals - same convention as the pre-69f3f97 client.
psql_insert_event_sql(NextId, ScreenName, Payload, SQL) :-
    event_table(EventTable),
    append(
        [
            "INSERT INTO public.", EventTable, " (",
            "id, screen_name, payload, occurred_at, started_at, ended_at",
            ") VALUES (",
            "'", NextId, "', ",
            "'", ScreenName, "', ",
            "'", Payload, "', ",
            "NOW(), NOW(), NOW()",
            ");"
        ],
        SQL
    ).

psql_select_event_payload_sql(ScreenName, SQL) :-
    event_table(EventTable),
    append(
        [
            "SELECT e.payload AS payload ",
            "FROM public.", EventTable, " e ",
            "WHERE e.screen_name::text = '", ScreenName, "';"
        ],
        SQL
    ).

insert_event_sql(SQL) :-
    event_table(EventTable),
    append(
        [
            "INSERT INTO public.", EventTable, " (",
            "id, screen_name, payload, occurred_at, started_at, ended_at",
            ") VALUES ($1, $2, $3, NOW(), NOW(), NOW())"
        ],
        SQL
    ).

select_event_payload_sql(SQL) :-
    event_table(EventTable),
    append(
        [
            "SELECT e.payload AS payload ",
            "FROM public.", EventTable, " e ",
            "WHERE e.screen_name::text = $1;"
        ],
        SQL
    ).

psql_count_matching_events_sql(ScreenName, SQL) :-
    event_table(EventTable),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", EventTable, " t ",
        "WHERE t.screen_name::text = '", ScreenName, "';"
    ], SQL).

count_matching_events_sql(SQL) :-
    event_table(EventTable),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", EventTable, " t ",
        "WHERE t.screen_name::text = $1;"
    ], SQL).

%% psql_insert_record_sql(+NextIdChars, +EncodedDescription, +Handle,
%%                        +Did, +Avatar, +EncodedDisplayName, -SQL).
%
% Concat form of the weaving_user INSERT for the psql backend.
% EncodedDescription and EncodedDisplayName arrive base64-encoded
% from encode_field_value/2; the other identifiers (Handle, Did,
% Avatar, NextIdChars) are inlined as single-quoted SQL literals,
% same convention as the pre-69f3f97 client.
psql_insert_record_sql(
    NextIdChars,
    EncodedDescription,
    Handle,
    Did,
    Avatar,
    EncodedDisplayName,
    SQL
) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "usr_id, usr_api_key, max_status_id, min_status_id, ",
            "max_like_id, min_like_id, total_statuses, total_likes, ",
            "description, url, last_status_publication_date, ",
            "total_subscribees, total_subscriptions, ",
            "usr_twitter_id, usr_twitter_username, usr_avatar, ",
            "usr_full_name, usr_status, usr_user_name, ",
            "usr_username_canonical, usr_email, usr_email_canonical, ",
            "protected, suspended, not_found, usr_position_in_hierarchy",
            ") VALUES (",
            NextIdChars, ", NULL, NULL, NULL, ",
            "NULL, NULL, 0, 0, ",
            "'", EncodedDescription, "', ",
            "'", Handle, "', NULL, ",
            "0, 0, ",
            "'", Did, "', ",
            "'", Handle, "', ",
            "'", Avatar, "', ",
            "'", EncodedDisplayName, "', false, ",
            "'", Handle, "', ",
            "'", Handle, "', '', '', ",
            "false, false, false, 1",
            ");"
        ],
        SQL
    ).

psql_select_record_id_sql(Did, SQL) :-
    table(Table),
    append(
        [
            "SELECT r.usr_id AS identifier ",
            "FROM public.", Table, " r ",
            "WHERE r.usr_twitter_id::text = '", Did, "' ",
            "ORDER BY r.usr_id DESC LIMIT 1;"
        ],
        SQL
    ).

insert_record_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "usr_id, usr_api_key, max_status_id, min_status_id, ",
            "max_like_id, min_like_id, total_statuses, total_likes, ",
            "description, url, last_status_publication_date, ",
            "total_subscribees, total_subscriptions, ",
            "usr_twitter_id, usr_twitter_username, usr_avatar, ",
            "usr_full_name, usr_status, usr_user_name, ",
            "usr_username_canonical, usr_email, usr_email_canonical, ",
            "protected, suspended, not_found, usr_position_in_hierarchy",
            ") VALUES (",
            "$1::bigint, NULL, NULL, NULL, ",
            "NULL, NULL, 0, 0, ",
            "$2, $3, NULL, ",
            "0, 0, ",
            "$4, $5, $6, ",
            "$7, false, $8, ",
            "$9, '', '', ",
            "false, false, false, 1",
            ")"
        ],
        SQL
    ).

select_record_id_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT r.usr_id AS identifier ",
            "FROM public.", Table, " r ",
            "WHERE r.usr_twitter_id::text = $1 ",
            "ORDER BY r.usr_id DESC LIMIT 1;"
        ],
        SQL
    ).

psql_count_matching_records_sql(Did, SQL) :-
    table(Table),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", Table, " r ",
        "WHERE r.usr_twitter_id::text = '", Did, "';"
    ], SQL).

count_matching_records_sql(SQL) :-
    table(Table),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", Table, " r ",
        "WHERE r.usr_twitter_id::text = $1;"
    ], SQL).
