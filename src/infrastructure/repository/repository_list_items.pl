:- module(repository_list_items, [
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
:- use_module('../../logger', [log_info/1]).
:- use_module('../../os_ext', [remove_temporary_file/1]).
:- use_module('../../serialization', [
    pairs_to_assoc/2,
    to_json_chars/2
]).
:- use_module('../../stream', [read_stream/2]).
:- use_module('../../temporal', [date_iso8601/1]).

%% count(-Count).
count(Count) :-
    table(Table),
    append(["select count(*) as Count from public.", Table], Query),
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
    from_clause(FromClause),

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
            "m.usr_id DESC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        Query
    ).

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
            "m.usr_id::bigint AS number__id, ",
            % handle
            "m.usr_twitter_username::text AS string__handle, ",
            % did
            "m.usr_twitter_id::text AS string__did, ",
            "COALESCE(m.usr_user_name::text, m.usr_twitter_username::text, ' ') AS string__username, ",
            % avatar
            "COALESCE(m.usr_avatar::text, ' ') AS string__avatar, ",
            % uri
            "COALESCE(m.url::text, ' ') AS string__uri, ",
            % indexedAt
            "COALESCE(m.last_status_publication_date::text, ' ') AS string__indexed_at "
        ],
        SelectClause
    ).

%% from_clause(-FromClause).
from_clause(FromClause) :-
    table(Table),
    append(["FROM public.", Table, " m "], FromClause).

%% read_rows(+TmpFile, -Rows).
read_rows(TmpFile, Rows) :-
    once(open(TmpFile, read, Stream, [type(text)])),
    (   file_exists(TmpFile)
    ->  true
    ;   write(file_does_not_exist), halt ),

    read_stream(Stream, StreamRows),
    once(phrase(rows(Rows), StreamRows, [])),
    remove_temporary_file(TmpFile).

%% count_matching_records(+NextId, -Result).
count_matching_records(ScreenName, Result) :-
    event_table(EventTable),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", EventTable, " ",
        "WHERE screen_name::text = ", ScreenName, ";"
    ], SelectQuery),
    once(query_result(
        SelectQuery,
        Result
    )).

%% insert(+Row, -InsertionResult).
insert(row(ScreenName, Payload), InsertionResult) :-
    event_table(EventTable),
    count_matching_records(ScreenName, TotalMatchingRecords),

    if_(
        dif(1, TotalMatchingRecords),
        (uuidv4_string(NextId),
        append(
            [
                "INSERT INTO public.", EventTable, " (",
                "   id,                 ",
                "   screen_name,          ",
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
                "SELECT m.payload AS payload ",
                "FROM public.", EventTable," m ",
                "WHERE m.screen_name::text = ", ScreenName, ";"
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
        log_info([JSONChars]),
        InsertionResult = ok)
    ).

%% event_table(-EventTable).
event_table("member_profile_collected_event").

%% table(-Table).
table("weaving_user").
