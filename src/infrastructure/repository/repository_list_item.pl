:- module(repository_list_item, [
    event_by_screen_name/2,
    from_event/2,
    record_by_screen_name/2,
    count/1,
    insert/2,
    insert_list_items_if_not_exists/2,
    insert_record/2,
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
    query_result_from_file/4,
    read_rows/2,
    value/3
]).
:- use_module('../../logger', [
    log_error/1,
    log_debug/1,
    log_info/1
]).
:- use_module('../../os_ext', [remove_temporary_file/1]).
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
the rest of the repository layer: `count/1`, `query/1`,
`insert/2`, `next_id/1`, plus screen-name lookups.
*/

%% table(-Table).
table("weaving_user").

%% event_table(-EventTable).
event_table("member_profile_collected_event").

%% count(-Count)
%
% Total number of rows in `weaving_user`.
count(Count) :-
    count_sql(SQL),
    value(SQL, [], Count).

count_sql(SQL) :-
    table(Table),
    append(["SELECT count(*) AS matching_records_count FROM public.", Table], SQL).

%% query(-HeadersAndRows)
%
% First 15 rows of `weaving_user` as header-keyed assocs.
query(HeadersAndRows) :-
    listing_headers(Headers),
    query(Rows, [], 15),
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
            "COALESCE(r.usr_avatar::text, ' ') AS string__avatar, ",
            "r.description::bytea AS string__description, ",
            "COALESCE(r.url::text, ' ') AS string__uri, ",
            "COALESCE(r.last_status_publication_date::text, ' ') AS string__indexed_at ",
            "FROM public.", Table, " r ",
            "ORDER BY r.usr_id DESC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        SQL
    ).

%% query(-Rows, +Headers, +Limit).
query(Rows, Headers, Limit) :-
    once(all_clauses(SQL, Params, Limit)),
    once(query_result_from_file(SQL, Params, Headers, TmpFile)),
    read_rows(TmpFile, Rows).

%% next_id(-NextId)
%
% Next available `usr_id` for `weaving_user`.
next_id(NextId) :-
    query_max_id_sql(SQL),
    value(SQL, [], MaxIdValue),
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

%% event_by_screen_name(+ScreenName, -HeadersAndRows)
%
% Multi-row read of `member_profile_collected_event` for the
% given handle, restricted to events from 2025 onward.
event_by_screen_name(ScreenName, HeadersAndRows) :-
    chars_si(ScreenName),
    event_by_screen_name_headers(Headers),
    event_by_screen_name_sql(SQL),
    log_debug([SQL]),
    once(query_result_from_file(SQL, [ScreenName], [], TmpFile)),
    once(read_rows_or_throw_screen_name(TmpFile, Rows)),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

read_rows_or_throw_screen_name(TmpFile, Rows) :- read_rows(TmpFile, Rows).
read_rows_or_throw_screen_name(_, _) :- throw(cannot_read_rows_selected_by(screen_name)).

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

%% record_by_screen_name(+ScreenName, -HeadersAndRows)
%
% Full denormalised `weaving_user` row for the given handle,
% returned as a single-row list of header-keyed assocs.
record_by_screen_name(ScreenName, HeadersAndRows) :-
    chars_si(ScreenName),
    record_by_screen_name_headers(Headers),
    record_by_screen_name_sql(SQL),
    log_debug([SQL]),
    once(query_result_from_file(SQL, [ScreenName], [], TmpFile)),
    once(read_rows_or_throw_screen_name(TmpFile, Rows)),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

record_by_screen_name_headers([
    "number__id",
    "string__api_key",
    "number__max_status_id",
    "number__min_status_id",
    "number__max_status_id",
    "number__min_status_id",
    "number__total_statuses",
    "number__total_likes",
    "string__id",
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

record_by_screen_name_sql(SQL) :-
    table(Table),
    append([
        "SELECT ",
        "r.usr_id AS number__id, ",
        "r.usr_api_key AS string__api_key, ",
        "r.max_status_id AS number__max_status_id, ",
        "r.min_status_id AS number__min_status_id, ",
        "r.max_like_id AS number__max_status_id, ",
        "r.min_like_id AS number__min_status_id, ",
        "r.total_statuses AS number__total_statuses, ",
        "r.total_likes AS number__total_likes, ",
        "r.description AS string__id, ",
        "r.url AS string__url, ",
        "r.last_status_publication_date AS string__last_status_publication_date, ",
        "r.total_subscribees AS number__total_subscribees, ",
        "r.total_subscriptions AS number__total_subscriptions, ",
        "r.usr_twitter_id AS string__twitter_id, ",
        "r.usr_twitter_username AS string__twitter_username, ",
        "r.usr_avatar AS string__avatar, ",
        "r.usr_full_name AS string__full_name, ",
        "r.usr_status AS boolean__status, ",
        "r.usr_user_name AS string__user_name, ",
        "r.usr_username_canonical AS string__user_name_canonical, ",
        "r.usr_email AS string__email, ",
        "r.usr_email_canonical AS string__email_canonical, ",
        "r.protected AS boolean__protected, ",
        "r.suspended AS boolean__suspended, ",
        "r.not_found AS boolean__not_found, ",
        "r.usr_position_in_hierarchy AS number__position_in_hierarchy ",
        "FROM public.", Table, " r ",
        "WHERE r.usr_twitter_username = $1 ",
        "OFFSET 0 LIMIT ALL;"
    ], SQL).

%% insert(+Row, -InsertionResult)
%
% Insert a new `member_profile_collected_event` for
% `ScreenName` carrying `Payload`, or log the existing payload
% if an event already exists. Always succeeds with `ok`.
insert(row(ScreenName, Payload), InsertionResult) :-
    count_matching_events(ScreenName, TotalMatchingEvents),
    if_(
        dif(1, TotalMatchingEvents),
        insert_new_event(ScreenName, Payload, InsertionResult),
        report_existing_event(ScreenName, InsertionResult)
    ).

insert_new_event(ScreenName, Payload, ok) :-
    uuidv4_string(NextId),
    insert_event_sql(SQL),
    execute(SQL, [NextId, ScreenName, Payload]).

report_existing_event(ScreenName, ok) :-
    select_event_payload_sql(SelectSQL),
    value(SelectSQL, [ScreenName], SelectionResult),
    log_existing_event_payload(SelectionResult).

log_existing_event_payload(no_records_found) :-
    log_debug([no_existing_event_payload]).
log_existing_event_payload(SelectionResult) :-
    dif(SelectionResult, no_records_found),
    chars_base64(DecodedBase64Payload, SelectionResult, []),
    maplist(char_code, DecodedBase64Payload, DecodedBytes),
    chars_utf8bytes(DecodedChars, DecodedBytes),
    append([Prefix, ['"']], DecodedChars),
    append([['"'], Suffix], Prefix),
    to_json_chars(Suffix, JSONChars),
    log_debug([JSONChars]).

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

%% count_matching_events(+ScreenName, -Result).
count_matching_events(ScreenName, Result) :-
    count_matching_events_sql(SQL),
    value(SQL, [ScreenName], Result).

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

%% insert_record(+Row, -InsertionResult)
%
% Insert a new `weaving_user` row for the given handle, or log
% the existing record's id if one already exists.
insert_record(
    row(
        Avatar,
        Description,
        Did,
        Handle,
        DisplayName
    ),
    InsertionResult
) :-
    count_matching_records(Handle, TotalMatchingRecords),
    write_term(total: TotalMatchingRecords, [double_quotes(true)]), nl,
    if_(
        dif(1, TotalMatchingRecords),
        insert_new_record(Avatar, Description, Did, Handle, DisplayName, InsertionResult),
        report_existing_record(Handle, InsertionResult)
    ).

insert_new_record(Avatar, Description, Did, Handle, DisplayName, ok) :-
    next_id(NextId),
    write_term(next_id: NextId, [double_quotes(true)]), nl,
    number_chars(NextId, NextIdChars),
    encode_field_value(DisplayName, EncodedDisplayName),
    encode_field_value(Description, EncodedDescription),
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
        execute(SQL, Params),
        E,
        throw(cannot_insert_record_into_database(E))
    ),
    write_term(record_insertion_result(ok), [double_quotes(true)]), nl.

report_existing_record(Handle, ok) :-
    select_record_id_sql(SelectSQL),
    value(SelectSQL, [Handle], SelectionResult),
    write_term(selection:SelectionResult, [double_quotes(true)]).

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
            "WHERE r.usr_twitter_username::text = $1;"
        ],
        SQL
    ).

%% count_matching_records(+Handle, -Result).
count_matching_records(Handle, Result) :-
    count_matching_records_sql(SQL),
    value(SQL, [Handle], Result).

count_matching_records_sql(SQL) :-
    table(Table),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", Table, " r ",
        "WHERE r.usr_user_name::text = $1;"
    ], SQL).

%% insert_list_items_if_not_exists(+Row, -HeadersAndRowsSelectedByHandle)
%
% Composite of `record_by_screen_name/2` and
% `insert_record/2`: try to read the existing record first;
% on a miss, fall through to insert and log the outcome.
insert_list_items_if_not_exists(
    row(
        Avatar,
        Description,
        Did,
        Handle,
        DisplayName
    ),
    HeadersAndRowsSelectedByHandle
) :-
    catch(
        record_by_screen_name(Handle, HeadersAndRowsSelectedByHandle),
        E,
        (log_info([E]),
        catch(
            (insert_record(
                row(
                    Avatar,
                    Description,
                    Did,
                    Handle,
                    DisplayName
                ),
                InsertionResult
            ),
            writeln(member_record_insertion_result(InsertionResult), true)),
            Err,
            log_error([error_on_record_insertion(Err)])
        ))
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
    get_assoc(displayName, Subject, DisplayName),
    writeln(displayName:DisplayName),
    get_assoc(did, Subject, DID),
    writeln(did:DID),
    get_assoc(handle, Subject, Handle),
    writeln(handle:Handle),
    get_assoc(createdAt, Subject, CreatedAt),
    writeln(createdAt:CreatedAt),
    get_assoc(indexedAt, Subject, IndexedAt),
    writeln(indexedAt:IndexedAt),
    catch(
        ( extract_associated_chat(Subject),
          extract_or_default_description(Subject, Description) ),
        key_not_found(Field),
        writeln(not_found(Field))
    ).

extract_associated_chat(Subject) :-
    get_assoc(associated, Subject, Associated),
    get_assoc(chat, Associated, Chat),
    get_assoc(allowIncoming, Chat, AllowIncomingChat),
    writeln(associated:AllowIncomingChat).
extract_associated_chat(Subject) :-
    \+ get_assoc(associated, Subject, _),
    throw(key_not_found(associated)).

extract_or_default_description(Subject, Description) :-
    get_assoc(description, Subject, Description),
    writeln(description:Description).
extract_or_default_description(Subject, "") :-
    \+ get_assoc(description, Subject, _).
