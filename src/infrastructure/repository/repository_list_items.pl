:- module(repository_list_items, [
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
:- use_module(library(reif)).
:- use_module(library(serialization/json)).
:- use_module(library(si)).
:- use_module(library(uuid)).

:- use_module('repository_dcgs', [
    rows//1,
    to_json/3
]).
:- use_module('../pg/client', [
    query_result/2,
    query_result_from_file/3
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

%% count(-Count).
count(Count) :-
    table(Table),
    append(["SELECT count(*) AS Count FROM public.", Table], Query),
    query_result(
        Query,
        Count
    ).

%% query(-HeadersAndRows).
query(HeadersAndRows) :-
    % Executed to fetch column names
    query(EmptyResults, false, 0),
    nth0(0, EmptyResults, Headers),
    query(Rows, true, 15),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

%% Query max list id, then increment it by 1 to declare what next list id is
%
%% next_id(-NextId).
next_id(NextId) :-
    query_max_id(Rows),
    nth0(0, Rows, Headers),
    nth0(0, Headers, SingleField),
    number_chars(MaxId, SingleField),
    NextId #= MaxId + 1,
    (   integer_si(NextId)
    ->  true
    ;   throw(invalid_list_id(NextId)) ).

%% all_clauses(-Query, +Limit).
all_clauses(Query, Limit) :-
    select_clause(SelectClause),
    from_record_table_clause(FromClause),

    % Accepting "ALL" as limit
    % and sufficiently instantiated integers
    ( ( integer_si(Limit),
        number_chars(Limit, LimitClause) )
    ->  true
    ;   Limit = "ALL",
        LimitClause = Limit ),

    append(
        SelectClause,
        FromClause,
        SelectFromClauses
    ),

    append(
        [
            SelectFromClauses,
            "ORDER BY ",
            "r.usr_id DESC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        Query
    ).

    %% from_record_table_clause(-FromClause).
    from_record_table_clause(FromClause) :-
        table(Table),
        append(["FROM public.", Table, " r "], FromClause).

%% query(-Rows, +TuplesOnly, +Limit).
query(Rows, TuplesOnly, Limit) :-
    all_clauses(Query, Limit),
    once(query_result_from_file(
        Query,
        TuplesOnly,
        TmpFile
    )),
    read_rows(TmpFile, Rows).

%% query_max_id(-MaxId).
query_max_id(MaxId) :-
    table(Table),
    append(
        [
            "SELECT u.usr_id pk ",
            "FROM ", Table, " u ",
            "ORDER BY pk DESC ",
            "LIMIT 1;"
        ],
        Query
    ),
    once(query_result_from_file(
        Query,
        true,
        TmpFile
    )),
    read_rows(TmpFile, MaxId).

%% select_clause(-SelectClause).
select_clause(SelectClause) :-
    append(
        [
            "SELECT ",
            "r.usr_id::bigint AS number__id, ",
            % handle
            "r.usr_twitter_username::text AS string__handle, ",
            % did
            "r.usr_twitter_id::text AS string__did, ",
            "COALESCE(r.usr_user_name::text, r.usr_twitter_username::text, ' ') AS string__username, ",
            % avatar
            "COALESCE(r.usr_avatar::text, ' ') AS string__avatar, ",
            % description
            "r.description::bytea AS string__description, ",
            % uri
            "COALESCE(r.url::text, ' ') AS string__uri, ",
            % indexedAt
            "COALESCE(r.last_status_publication_date::text, ' ') AS string__indexed_at "
        ],
        SelectClause
    ).

%% event_by_screen_name(+ScreenName, -HeadersAndRows).
event_by_screen_name(ScreenName, HeadersAndRows) :-
    chars_si(ScreenName),
    from_event_table_clause(FromClause),
    append([
        "SELECT ",
        "payload as string__payload, ",
        "screen_name as string__screen_name ",
        FromClause, " e ",
        "WHERE ",
        "e.occurred_at::date > '2024-12-31'::date ",
        "AND e.screen_name = '", ScreenName, "' ",
        "OFFSET 0 "
    ], SelectByScreenName),
    append(
        [
            SelectByScreenName,
            "LIMIT 0;"
        ],
        QueryHeaders
    ),
    once(query_result_from_file(
        QueryHeaders,
        false,
        HeadersOnlyTempFile
    )),
    read_rows(HeadersOnlyTempFile, HeadersRows),

    nth0(0, HeadersRows, Headers),
    append(
        [
            SelectByScreenName,
            "LIMIT ALL;"
        ],
        SelectByScreenNameWithoutLimit
    ),
    log_debug([SelectByScreenNameWithoutLimit]),
    once(query_result_from_file(
        SelectByScreenNameWithoutLimit,
        true,
        ByScreenNameTmpFile
    )),
    (   read_rows(ByScreenNameTmpFile, Rows)
    ->  true
    ;   throw(cannot_read_rows_selected_by(screen_name)) ),

    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

%% record_by_screen_name(+ScreenName, -HeadersAndRows).
record_by_screen_name(ScreenName, HeadersAndRows) :-
    chars_si(ScreenName),
    from_record_table_clause(FromClause),
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
        FromClause,
        "WHERE ",
        "r.usr_twitter_username = '", ScreenName, "' ",
        "OFFSET 0 "
    ], SelectByScreenName),
    append(
        [
            SelectByScreenName,
            "LIMIT 0;"
        ],
        QueryHeaders
    ),
    once(query_result_from_file(
        QueryHeaders,
        false,
        HeadersOnlyTempFile
    )),
    read_rows(HeadersOnlyTempFile, HeadersRows),

    nth0(0, HeadersRows, Headers),
    append(
        [
            SelectByScreenName,
            "LIMIT ALL;"
        ],
        SelectByScreenNameWithoutLimit
    ),
    log_debug([SelectByScreenNameWithoutLimit]),
    once(query_result_from_file(
        SelectByScreenNameWithoutLimit,
        true,
        ByScreenNameTmpFile
    )),
    (   read_rows(ByScreenNameTmpFile, Rows)
    ->  true
    ;   throw(cannot_read_rows_selected_by(screen_name)) ),

    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

from_event_table_clause(FromClause) :-
    event_table(EventTable),
    append(["FROM public.", EventTable, " "], FromClause).

%% read_rows(+TmpFile, -Rows).
read_rows(TmpFile, Rows) :-
    once(open(TmpFile, read, Stream, [type(text)])),
    (   file_exists(TmpFile)
    ->  true
    ;   write(file_does_not_exist), halt ),

    read_stream(Stream, StreamRows),
    once(phrase(rows(Rows), StreamRows, [])),
    remove_temporary_file(TmpFile).

%% insert(+Row, -InsertionResult).
insert(row(ScreenName, Payload), InsertionResult) :-
    event_table(EventTable),
    count_matching_events(ScreenName, TotalMatchingEvents),

    if_(
        dif(1, TotalMatchingEvents),
        (uuidv4_string(NextId),
        append(
            [
                "INSERT INTO public.", EventTable, " (",
                "   id,                 ",
                "   screen_name,        ",
                "   payload,            ",
                "   occurred_at,        ",
                "   started_at,         ",
                "   ended_at            ",
                ") VALUES (             ",
                "'", NextId, "',        ",
                "'", ScreenName, "',        ",
                "'", Payload, "',       ",
                "   NOW(),              ",
                "   NOW(),              ",
                "   NOW()               ",
                ")"
            ],
            Query
        ),
        once(query_result(
            Query,
            InsertionResult
        ))),
        (append(
            [
                "SELECT e.payload AS payload ",
                "FROM public.", EventTable," e ",
                "WHERE e.screen_name::text = '", ScreenName, "';"
            ],
            SelectQuery
        ),
        once(query_result(
            SelectQuery,
            SelectionResult
        )),
        chars_base64(DecodedBase64Payload, SelectionResult, []),

        maplist(char_code, DecodedBase64Payload, DecodedBytes),
        chars_utf8bytes(DecodedChars, DecodedBytes),
        append([Prefix, ['"']], DecodedChars),
        append([['"'], Suffix], Prefix),
        to_json_chars(Suffix, JSONChars),
        log_debug([JSONChars]),
        InsertionResult = ok)
    ).

    %% count_matching_events(+ScreenName, -Result).
    count_matching_events(ScreenName, Result) :-
        from_event_table_clause(FromClause),
        append([
            "SELECT count(*) how_many_records ",
            FromClause, " t ",
            "WHERE t.screen_name::text = '", ScreenName, "';"
        ], SelectQuery),
        once(query_result(
            SelectQuery,
            Result
        )).

encode_field_value(FieldValue, EncodedFieldValue) :-
        write_term_to_chars(FieldValue, [quoted(true), double_quotes(true)], QuotedFieldValue),
        chars_utf8bytes(QuotedFieldValue, Utf8Bytes),
        maplist(char_code_at, Utf8Bytes, FieldUtf8Bytes),
        chars_base64(FieldUtf8Bytes, EncodedFieldValue, []).

%% insert_record(+Row, -InsertionResult).
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
    table(Table),
    count_matching_records(Handle, TotalMatchingRecords),
    write_term(total: TotalMatchingRecords, [double_quotes(true)]), nl,

    if_(
        dif(1, TotalMatchingRecords),
        (next_id(NextId),
        write_term(next_id: NextId, [double_quotes(true)]), nl,
        % Convert NextId to a list of characters
        % so that it could be added  to insertion query
        number_chars(NextId, NextIdChars),

        encode_field_value(DisplayName, EncodedDisplayName),
        encode_field_value(Description, EncodedDescription),

        append(
            [
                "INSERT INTO public.", Table, " (   ",
                "   usr_id,                        ",
                "   usr_api_key,                    ",
                "   max_status_id,                  ",
                "   min_status_id,                  ",
                "   max_like_id,                    ",
                "   min_like_id,                    ",
                "   total_statuses,                 ",
                "   total_likes,                    ",
                "   description,                    ",
                "   url,                            ",
                "   last_status_publication_date,   ",
                "   total_subscribees,              ",
                "   total_subscriptions,            ",
                "   usr_twitter_id,                 ",
                "   usr_twitter_username,           ",
                "   usr_avatar,                     ",
                "   usr_full_name,                  ",
                "   usr_status,                     ",
                "   usr_user_name,                  ",
                "   usr_username_canonical,         ",
                "   usr_email,                      ",
                "   usr_email_canonical,            ",
                "   protected,                      ",
                "   suspended,                      ",
                "   not_found,                      ",
                "   usr_position_in_hierarchy       ",
                ") VALUES (                         ",
                "", NextIdChars, ",                 ", % usr_id
                "   NULL,                           ",
                "   NULL,                           ",
                "   NULL,                           ",
                "   NULL,                           ",
                "   NULL,                           ",
                "   0,                              ",
                "   0,                              ",
                "'", EncodedDescription, "',         ", % description
                "'", Handle, "',                    ", % url
                "   NULL,                           ",
                "   0,                              ",
                "   0,                              ",
                "'", Did, "',                       ", % usr_twitter_id
                "'", Handle, "',                    ", % usr_twitter_username
                "'", Avatar, "',                    ", % usr_avatar
                "'", EncodedDisplayName, "',        ", % usr_full_name
                "   false,  ",
                "'", Handle, "',                    ", % usr_user_name
                "'", Handle, "',                    ", % usr_username_canonical
                "   '',                             ", % usr_email
                "   '',                             ", % usr_email_canonical
                "   false,                          ",
                "   false,                          ",
                "   false,                          ",
                "   1                               ",
                ")"
            ],
            InsertRecordQuery
        ),
        log_debug([query:InsertRecordQuery]),

        (   once(query_result(
                InsertRecordQuery,
                InsertionResult)
            )
        ->  write_term(record_insertion_result(InsertionResult), [double_quotes(true)]), nl
        ;   throw(cannot_insert_record_into_database(InsertRecordQuery)) ) ),

        (append(
            [
                "SELECT r.usr_id AS identifier ",
                "FROM public.", Table," r ",
                "WHERE r.usr_twitter_username::text = '", Handle, "';"
            ],
            SelectQuery
        ),
        once(query_result(
            SelectQuery,
            SelectionResult
        )),

        write_term(selection:SelectionResult, [double_quotes(true)])

%       chars_base64(DecodedBase64Payload, SelectionResult, []),
%       maplist(char_code, DecodedBase64Payload, DecodedBytes),
%       chars_utf8bytes(DecodedChars, DecodedBytes),
%       append([Prefix, ['"']], DecodedChars),
%       append([['"'], Suffix], Prefix),
%       to_json_chars(Suffix, JSONChars),
%       log_debug([JSONChars]),
%        InsertionResult = ok
        )
    ).

    %% count_matching_records(+Handle, -Result).
    count_matching_records(Handle, Result) :-
        from_record_table_clause(FromClause),
        append([
            "SELECT count(*) how_many_records ",
            FromClause,
            "WHERE r.usr_user_name::text = '", Handle, "';"
        ], SelectQuery),
        once(query_result(
            SelectQuery,
            Result
        )).

%% insert_list_items_if_not_exists(+Row -HeadersAndRowsSelectedByHandle).
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

%% event(+Payload, -Row).
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

    once(catch(
        ( ( get_assoc(associated, Subject, Associated),
            get_assoc(chat, Associated, Chat),
            get_assoc(allowIncoming, Chat, AllowIncomingChat),
            writeln(associated:AllowIncomingChat)
        ;   throw(key_not_found(associated)) ),
        ( get_assoc(description, Subject, Description),
            writeln(description:Description)
        ;   Description = "" )),
        key_not_found(Field),
        writeln(not_found(Field))
    )).

%% event_table(-EventTable).
event_table("member_profile_collected_event").

%% table(-Table).
table("weaving_user").
