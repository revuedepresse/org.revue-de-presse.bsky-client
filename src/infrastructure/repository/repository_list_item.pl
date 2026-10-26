:- module(repository_list_item, [
    event_by_screen_name/3,
    from_event/2,
    record_by_did/3,
    record_by_screen_name/3,
    count/2,
    insert/3,
    insert_list_items_if_not_exists/3,
    insert_record/3,
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
    execute/3,
    matching_criteria/2,
    query_result/2,
    query_result_from_file/3,
    query_result_from_file/5,
    read_rows/2,
    value/4
]).
:- use_module('../../configuration', [pg_backend/1]).
:- use_module('../../logger', [
    log_error/1,
    log_debug/1,
    log_info/1
]).
:- use_module('../../os_ext', [remove_temporary_file/1]).
:- use_module('../../clean_text', [clean_text/2]).
:- use_module('../../serialization', [
    char_code_at/2,
    pairs_to_assoc/2,
    to_json_chars/2
]).
:- use_module('../../stream', [
    read_stream/2,
    writeln/1,
    writeln/2
]).
:- use_module('../../temporal', [date_iso8601/1]).

/**
Repository for individual authors listed inside a curated list.

Reads and writes `weaving_user` (the canonical member records
that downstream callers select by handle) and
`member_profile_collected_event` (the append-only log of every
profile snapshot we've collected). The public surface mirrors
the rest of the repository layer: `count/2`, `query/2`,
`insert/3`, `next_id/2`, plus screen-name and DID lookups.

Every wire-path predicate takes a `pg_session(In, Out)` compound
as its first argument; psql-path clauses thread the session as a
passthrough (input = output) since the psql shell-out is
stateless.
*/

%% table(-Table).
table("weaving_user").

%% event_table(-EventTable).
event_table("member_profile_collected_event").

%% count(+Session, -Count)
%
% Total number of rows in `weaving_user`. Dispatches on PG_BACKEND.
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

%% query(+Session, -HeadersAndRows)
%
% First 15 rows of `weaving_user` as header-keyed assocs.
query(Session, HeadersAndRows) :-
    listing_headers(Headers),
    query(Session, Rows, [], 15),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

listing_headers([
    "number__id",
    "string__handle",
    "string__did",
    "string__username",
    "string__avatar",
    "string__description",
    "string__uri",
    "string__indexed_at"
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

%% query(+Session, -Rows, +Headers, +Limit). Dispatches on PG_BACKEND.
query(pg_session(C0, C), Rows, Headers, Limit) :-
    pg_backend("wire"),
    once(all_clauses(SQL, Params, Limit)),
    once(query_result_from_file(pg_session(C0, C), SQL, Params, Headers, TmpFile)),
    read_rows(TmpFile, Rows).
query(pg_session(C, C), Rows, _Headers, Limit) :-
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

%% next_id(+Session, -NextId)
%
% Next available `usr_id` for `weaving_user`. Dispatches on PG_BACKEND.
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
            "SELECT u.usr_id pk ",
            "FROM ", Table, " u ",
            "ORDER BY pk DESC ",
            "LIMIT 1;"
        ],
        SQL
    ).

%% event_by_screen_name(+Session, +ScreenName, -HeadersAndRows)
%
% Multi-row read of `member_profile_collected_event` for the
% given handle, restricted to events from 2025 onward. Throws
% `cannot_read_rows_selected_by(screen_name)` when no event
% matches, which `event_getListItem` turns into an insert.
event_by_screen_name(pg_session(C0, C), ScreenName, HeadersAndRows) :-
    pg_backend("wire"),
    chars_si(ScreenName),
    event_by_screen_name_headers(Headers),
    event_by_screen_name_sql(SQL),
    log_debug([SQL]),
    once(query_result_from_file(pg_session(C0, C), SQL, [ScreenName], [], TmpFile)),
    once(read_rows_or_throw(TmpFile, screen_name, Rows)),
    rows_or_throw_on_miss(Rows, screen_name),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).
event_by_screen_name(pg_session(C, C), ScreenName, HeadersAndRows) :-
    pg_backend("psql"),
    chars_si(ScreenName),
    event_by_screen_name_headers(Headers),
    psql_event_by_screen_name_sql(ScreenName, SQL),
    log_debug([SQL]),
    once(query_result_from_file(SQL, true, TmpFile)),
    once(read_rows_or_throw(TmpFile, screen_name, Rows)),
    rows_or_throw_on_miss(Rows, screen_name),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

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

%% read_rows_or_throw(+TmpFile, +Selector, -Rows)
%
% `Selector` (`screen_name` or `did`) labels the throw, so callers
% can tell which lookup came back empty.
read_rows_or_throw(TmpFile, _, Rows) :- read_rows(TmpFile, Rows).
read_rows_or_throw(_, Selector, _) :- throw(cannot_read_rows_selected_by(Selector)).

%% rows_or_throw_on_miss(+Rows, +Selector)
%
% A query that matches nothing yields `[]` on both backends; turn
% that into the throw callers catch to take their insert path.
rows_or_throw_on_miss([], Selector) :-
    throw(cannot_read_rows_selected_by(Selector)).
rows_or_throw_on_miss([_|_], _).

event_by_screen_name_headers([
    "string__payload",
    "string__screen_name"
]).

event_by_screen_name_sql(SQL) :-
    event_table(EventTable),
    append([
        "SELECT payload AS string__payload, screen_name AS string__screen_name ",
        "FROM public.", EventTable, " e ",
        "WHERE e.occurred_at::date > '2024-12-31'::date ",
        "AND e.screen_name = $1 ",
        "OFFSET 0 LIMIT ALL;"
    ], SQL).

%% record_by_screen_name(+Session, +ScreenName, -HeadersAndRows)
%
% Full denormalised `weaving_user` row for the given handle,
% returned as a single-row list of header-keyed assocs. Throws
% `cannot_read_rows_selected_by(screen_name)` when no record
% matches.
record_by_screen_name(pg_session(C0, C), ScreenName, HeadersAndRows) :-
    pg_backend("wire"),
    chars_si(ScreenName),
    record_by_screen_name_headers(Headers),
    record_by_screen_name_sql(SQL),
    log_debug([SQL]),
    once(query_result_from_file(pg_session(C0, C), SQL, [ScreenName], [], TmpFile)),
    once(read_rows_or_throw(TmpFile, screen_name, Rows)),
    rows_or_throw_on_miss(Rows, screen_name),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).
record_by_screen_name(pg_session(C, C), ScreenName, HeadersAndRows) :-
    pg_backend("psql"),
    chars_si(ScreenName),
    record_by_screen_name_headers(Headers),
    psql_record_by_screen_name_sql(ScreenName, SQL),
    log_debug([SQL]),
    once(query_result_from_file(SQL, true, TmpFile)),
    once(read_rows_or_throw(TmpFile, screen_name, Rows)),
    rows_or_throw_on_miss(Rows, screen_name),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

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

record_by_screen_name_headers([
    "number__id",
    "string__api_key",
    "number__max_status_id",
    "number__min_status_id",
    "number__max_like_id",
    "number__min_like_id",
    "number__total_statuses",
    "number__total_likes",
    "string__description",
    "string__url",
    "string__last_status_publication_date",
    "number__total_subscribees",
    "number__total_subscriptions",
    "string__twitter_id",
    "string__twitter_username",
    "string__avatar",
    "string__full_name",
    "boolean__status",
    "string__user_name",
    "string__user_name_canonical",
    "string__email",
    "string__email_canonical",
    "boolean__protected",
    "boolean__suspended",
    "boolean__not_found",
    "number__position_in_hierarchy"
]).

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

%% record_by_did(+Session, +Did, -HeadersAndRows)
%
% Id and handle of the `weaving_user` rows recorded for the given
% DID (`usr_twitter_id`), most recent first. A handle can change on
% Bluesky, a DID cannot, so the DID identifies the member. Throws
% `cannot_read_rows_selected_by(did)` when no record matches.
% Several rows come back only for a DID recorded twice before the
% unique index on `usr_twitter_id`.
record_by_did(pg_session(C0, C), Did, HeadersAndRows) :-
    pg_backend("wire"),
    chars_si(Did),
    record_by_did_headers(Headers),
    record_by_did_sql(SQL),
    log_debug([SQL]),
    once(query_result_from_file(pg_session(C0, C), SQL, [Did], [], TmpFile)),
    once(read_rows_or_throw(TmpFile, did, Rows)),
    rows_or_throw_on_miss(Rows, did),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).
record_by_did(pg_session(C, C), Did, HeadersAndRows) :-
    pg_backend("psql"),
    chars_si(Did),
    record_by_did_headers(Headers),
    psql_record_by_did_sql(Did, SQL),
    log_debug([SQL]),
    once(query_result_from_file(SQL, true, TmpFile)),
    once(read_rows_or_throw(TmpFile, did, Rows)),
    rows_or_throw_on_miss(Rows, did),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

% The id is read as text: it goes back as the UPDATE's bind
% parameter (or psql literal) unchanged.
record_by_did_headers([
    "string__id",
    "string__twitter_username"
]).

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

%% update_handle_if_renamed(+Session, +Did, +Handle, +HeadersAndRows)
%
% A member whose rows (from `record_by_did/3`) all carry another
% handle has changed it on Bluesky: its most recent row takes the
% current one, and the rename is logged. A row already carrying the
% handle leaves the records as they are.
update_handle_if_renamed(Session, Did, Handle, HeadersAndRows) :-
    maplist(get_assoc(twitter_username), HeadersAndRows, RecordedHandles),
    if_(
        memberd_t(Handle, RecordedHandles),
        keep_session(Session),
        rename_record(Session, Did, Handle, HeadersAndRows)
    ).

keep_session(pg_session(C, C)).

rename_record(Session, Did, Handle, [MostRecentRow|_]) :-
    get_assoc(id, MostRecentRow, Id),
    get_assoc(twitter_username, MostRecentRow, PreviousHandle),
    update_record_handle(Session, Id, Handle),
    maplist(atom_chars, [DidAtom, PreviousHandleAtom, HandleAtom], [Did, PreviousHandle, Handle]),
    log_info([member_handle_renamed(did(DidAtom), from(PreviousHandleAtom), to(HandleAtom))]).

%% update_record_handle(+Session, +Id, +Handle). Dispatches on PG_BACKEND.
%
% Moves the `weaving_user` row `Id` to `Handle`: `usr_twitter_username`
% always, and the columns `insert_record/3` fills with the handle
% (`usr_user_name`, `usr_username_canonical`, `url`) when they still
% hold the previous one.
update_record_handle(pg_session(C0, C), Id, Handle) :-
    pg_backend("wire"),
    update_record_handle_sql(SQL),
    execute(pg_session(C0, C), SQL, [Handle, Id]).
update_record_handle(pg_session(C, C), Id, Handle) :-
    pg_backend("psql"),
    psql_update_record_handle_sql(Id, Handle, SQL),
    query_result(SQL, _Result).

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

%% insert(+Session, +Row, -InsertionResult)
%
% Insert a new `member_profile_collected_event` for
% `ScreenName` carrying `Payload`, or log the existing payload
% if an event already exists. Always succeeds with `ok`.
insert(pg_session(C0, C), row(ScreenName, Payload), InsertionResult) :-
    count_matching_events(pg_session(C0, C1), ScreenName, TotalMatchingEvents),
    if_(
        dif(1, TotalMatchingEvents),
        insert_new_event(pg_session(C1, C), ScreenName, Payload, InsertionResult),
        report_existing_event(pg_session(C1, C), ScreenName, InsertionResult)
    ).

insert_new_event(pg_session(C0, C), ScreenName, Payload, ok) :-
    pg_backend("wire"),
    uuidv4_string(NextId),
    insert_event_sql(SQL),
    execute(pg_session(C0, C), SQL, [NextId, ScreenName, Payload]).
insert_new_event(pg_session(C, C), ScreenName, Payload, ok) :-
    pg_backend("psql"),
    uuidv4_string(NextId),
    psql_insert_event_sql(NextId, ScreenName, Payload, SQL),
    query_result(SQL, _Result).

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

report_existing_event(pg_session(C0, C), ScreenName, ok) :-
    pg_backend("wire"),
    select_event_payload_sql(SelectSQL),
    value(pg_session(C0, C), SelectSQL, [ScreenName], SelectionResult),
    log_existing_event_payload(SelectionResult).
report_existing_event(pg_session(C, C), ScreenName, ok) :-
    pg_backend("psql"),
    psql_select_event_payload_sql(ScreenName, SelectSQL),
    query_result(SelectSQL, SelectionResult),
    log_existing_event_payload(SelectionResult).

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

% Profile events hold the item's JSON pairs written as a quoted
% Prolog term (see event_getListItem:onGetListItem/2), not a
% quoted JSON string, so the payload is logged as decoded.
log_existing_event_payload(no_records_found) :-
    log_debug([no_existing_event_payload]).
log_existing_event_payload(SelectionResult) :-
    dif(SelectionResult, no_records_found),
    chars_base64(DecodedBase64Payload, SelectionResult, []),
    maplist(char_code, DecodedBase64Payload, DecodedBytes),
    chars_utf8bytes(DecodedChars, DecodedBytes),
    log_debug([DecodedChars]).

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

%% count_matching_events(+Session, +ScreenName, -Result). Dispatches on PG_BACKEND.
count_matching_events(pg_session(C0, C), ScreenName, Result) :-
    pg_backend("wire"),
    count_matching_events_sql(SQL),
    value(pg_session(C0, C), SQL, [ScreenName], Result).
count_matching_events(pg_session(C, C), ScreenName, Result) :-
    pg_backend("psql"),
    psql_count_matching_events_sql(ScreenName, SQL),
    query_result(SQL, Result).

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

encode_field_value(FieldValue, EncodedFieldValue) :-
    write_term_to_chars(FieldValue, [quoted(true), double_quotes(true)], QuotedFieldValue),
    chars_utf8bytes(QuotedFieldValue, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, FieldUtf8Bytes),
    chars_base64(FieldUtf8Bytes, EncodedFieldValue, []).

%% insert_record(+Session, +Row, -InsertionResult)
%
% Insert a new `weaving_user` row for the given DID, or log the
% existing record's id if one already exists. Keyed by DID, not
% handle: a member that renamed itself, or took a handle another
% member dropped, must not get a second row for its DID.
insert_record(
    pg_session(C0, C),
    row(
        Avatar,
        Description,
        Did,
        Handle,
        DisplayName
    ),
    InsertionResult
) :-
    count_matching_records(pg_session(C0, C1), Did, TotalMatchingRecords),
    write_term(total: TotalMatchingRecords, [double_quotes(true)]), nl,
    if_(
        TotalMatchingRecords = 0,
        insert_new_record(pg_session(C1, C), Avatar, Description, Did, Handle, DisplayName, InsertionResult),
        report_existing_record(pg_session(C1, C), Did, InsertionResult)
    ).

insert_new_record(pg_session(C0, C), Avatar, Description, Did, Handle, DisplayName, ok) :-
    pg_backend("wire"),
    next_id(pg_session(C0, C1), NextId),
    write_term(next_id: NextId, [double_quotes(true)]), nl,
    number_chars(NextId, NextIdChars),
    clean_text(DisplayName, CleanedDisplayName),
    clean_text(Description, CleanedDescription),
    encode_field_value(CleanedDisplayName, EncodedDisplayName),
    encode_field_value(CleanedDescription, EncodedDescription),
    insert_record_sql(SQL),
    Params = [
        NextIdChars,
        EncodedDescription,
        Handle,
        Did,
        Handle,
        Avatar,
        EncodedDisplayName,
        Handle,
        Handle
    ],
    catch(
        execute(pg_session(C1, C), SQL, Params),
        E,
        throw(cannot_insert_record_into_database(E))
    ),
    write_term(record_insertion_result(ok), [double_quotes(true)]), nl.
insert_new_record(pg_session(C, C), Avatar, Description, Did, Handle, DisplayName, ok) :-
    pg_backend("psql"),
    next_id(pg_session(C, C), NextId),
    write_term(next_id: NextId, [double_quotes(true)]), nl,
    number_chars(NextId, NextIdChars),
    clean_text(DisplayName, CleanedDisplayName),
    clean_text(Description, CleanedDescription),
    encode_field_value(CleanedDisplayName, EncodedDisplayName),
    encode_field_value(CleanedDescription, EncodedDescription),
    psql_insert_record_sql(
        NextIdChars,
        EncodedDescription,
        Handle,
        Did,
        Avatar,
        EncodedDisplayName,
        SQL
    ),
    catch(
        query_result(SQL, _Result),
        E,
        throw(cannot_insert_record_into_database(E))
    ),
    write_term(record_insertion_result(ok), [double_quotes(true)]), nl.

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

report_existing_record(pg_session(C0, C), Did, ok) :-
    pg_backend("wire"),
    select_record_id_sql(SelectSQL),
    value(pg_session(C0, C), SelectSQL, [Did], SelectionResult),
    write_term(selection:SelectionResult, [double_quotes(true)]).
report_existing_record(pg_session(C, C), Did, ok) :-
    pg_backend("psql"),
    psql_select_record_id_sql(Did, SelectSQL),
    query_result(SelectSQL, SelectionResult),
    write_term(selection:SelectionResult, [double_quotes(true)]).

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

%% count_matching_records(+Session, +Did, -Result). Dispatches on PG_BACKEND.
count_matching_records(pg_session(C0, C), Did, Result) :-
    pg_backend("wire"),
    count_matching_records_sql(SQL),
    value(pg_session(C0, C), SQL, [Did], Result).
count_matching_records(pg_session(C, C), Did, Result) :-
    pg_backend("psql"),
    psql_count_matching_records_sql(Did, SQL),
    query_result(SQL, Result).

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

%% insert_list_items_if_not_exists(+Session, +Row, -HeadersAndRowsSelectedByDid)
%
% Composite of `record_by_did/3`, `update_handle_if_renamed/4` and
% `insert_record/3`: read the member's record by DID first; when
% found under another handle, move it to the current one; on a
% miss, fall through to insert and log the outcome. Any other
% error (a failed lookup, update or insert) is logged and leaves
% the session on its input connection, so the caller can carry on
% with the next item without inserting a second row for the DID.
insert_list_items_if_not_exists(
    pg_session(C0, C),
    row(
        Avatar,
        Description,
        Did,
        Handle,
        DisplayName
    ),
    HeadersAndRowsSelectedByDid
) :-
    catch(
        ensure_current_handle(pg_session(C0, C), Did, Handle, HeadersAndRowsSelectedByDid),
        E,
        on_record_lookup_error(
            pg_session(C0, C),
            row(
                Avatar,
                Description,
                Did,
                Handle,
                DisplayName
            ),
            E
        )
    ).

ensure_current_handle(pg_session(C0, C), Did, Handle, HeadersAndRowsSelectedByDid) :-
    record_by_did(pg_session(C0, C1), Did, HeadersAndRowsSelectedByDid),
    update_handle_if_renamed(pg_session(C1, C), Did, Handle, HeadersAndRowsSelectedByDid).

on_record_lookup_error(pg_session(C0, C), Row, E) :-
    if_(
        E = cannot_read_rows_selected_by(did),
        insert_missing_record(pg_session(C0, C), Row, E),
        ( log_error([error_on_record_lookup_or_update(E)]), C = C0 )
    ).

insert_missing_record(pg_session(C0, C), Row, Miss) :-
    log_info([Miss]),
    catch(
        ( once(insert_record(pg_session(C0, C), Row, InsertionResult)),
          writeln(member_record_insertion_result(InsertionResult), true) ),
        Err,
        ( log_error([error_on_record_insertion(Err)]), C = C0 )
    ).

%% from_event(+Payload, -Row)
%
% Decode a base64-encoded event payload as a list-item `row/5`
% with `Avatar`, `Description`, `DID`, `Handle`, and
% `DisplayName`, defaulting the description to `""` when the
% upstream profile omitted it.
from_event(Payload, row(Avatar, Description, DID, Handle, DisplayName)) :-
    chars_base64(Utf8BytesPayload, Payload, []),
    maplist(char_code, Utf8BytesPayload, Utf8Bytes),
    chars_utf8bytes(PayloadChars, Utf8Bytes),
    append(["pairs(", PayloadChars, ")."], CompleteReduction),
    read_from_chars(CompleteReduction, Term),
    Term = pairs(Pairs),
    pairs_to_assoc(Pairs, Assoc),
    get_assoc(subject, Assoc, Subject),
    assoc_to_keys(Subject, SubjectKeys),
    writeln('list items keys':SubjectKeys),
    get_assoc(avatar, Subject, Avatar),
    writeln(avatar:Avatar),
    display_name_or_handle(Subject, DisplayName),
    writeln(displayName:DisplayName),
    get_assoc(did, Subject, DID),
    writeln(did:DID),
    get_assoc(handle, Subject, Handle),
    writeln(handle:Handle),
    get_assoc(createdAt, Subject, CreatedAt),
    writeln(createdAt:CreatedAt),
    get_assoc(indexedAt, Subject, IndexedAt),
    writeln(indexedAt:IndexedAt),
    extract_associated_chat(Subject),
    extract_or_default_description(Subject, Description).

% A profile may have no display name; its handle stands in for it.
display_name_or_handle(Subject, DisplayName) :-
    get_assoc(displayName, Subject, DisplayName).
display_name_or_handle(Subject, Handle) :-
    \+ get_assoc(displayName, Subject, _),
    get_assoc(handle, Subject, Handle).

% Chat settings are only logged. Profiles may have no "associated"
% object, or one without "chat" (e.g. only "activitySubscription"):
% neither must stop the member from being recorded.
extract_associated_chat(Subject) :-
    allow_incoming_chat(Subject, AllowIncomingChat),
    writeln(associated:AllowIncomingChat).
extract_associated_chat(Subject) :-
    \+ allow_incoming_chat(Subject, _),
    writeln(not_found(associated_chat)).

allow_incoming_chat(Subject, AllowIncomingChat) :-
    get_assoc(associated, Subject, Associated),
    get_assoc(chat, Associated, Chat),
    get_assoc(allowIncoming, Chat, AllowIncomingChat).

extract_or_default_description(Subject, Description) :-
    get_assoc(description, Subject, Description),
    writeln(description:Description).
extract_or_default_description(Subject, "") :-
    \+ get_assoc(description, Subject, _).
