:- module(repository_lists, [
    by_list_uri/2,
    by_list_uri_or_throw/2,
    count/1,
    insert/2,
    next_event_id/1,
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
:- use_module('../../temporal', [date_iso8601/1]).

%% count(-Count).
count(Count) :-
    table(Table),
    append(
       ["select count(*) as Count from public.", Table],
       Query
    ),
    query_result(
        Query,
        Count
    ).

%% query(-HeadersAndRows).
query(HeadersAndRows) :-
    % Executed to fetch column names
    query(EmptyResults, false, 0),
    nth0(0, EmptyResults, Headers),
    query(Rows, true, "ALL"),
    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

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
            "t.public_id DESC, ",
            "t.name ASC, ",
            "t.screen_name ASC ",
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

%% Query max id, then increment it by 1 to declare what next max id will be.
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

    %% query_max_id(-MaxId).
    query_max_id(MaxId) :-
        table(Table),
        append(
            [
                "SELECT COALESCE(r.id::bigint, 0) pk ",
                "FROM ", Table, " r ",
                "ORDER BY r.id DESC ",
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

        %% table(-Table)
        table("publishers_list").

%% Query event max id, then increment it by 1 to declare what next event id is.
%
%% next_event_id(-NextEventId).
next_event_id(NextEventId) :-
    query_event_max_id(Rows),
    nth0(0, Rows, Headers),
    nth0(0, Headers, SingleField),
    number_chars(EventMaxId, SingleField),
    NextEventId #= EventMaxId + 1,
    (   integer_si(NextEventId)
    ->  true
    ;   throw(invalid_list_id(NextEventId)) ).

    %% query_event_max_id(-EventMaxId).
    query_event_max_id(EventMaxId) :-
        event_table(Table),
        append(
            [
                "SELECT COALESCE(e.list_id::bigint, 0) list ",
                "FROM ", Table, " e ",
                "ORDER BY list DESC ",
                "LIMIT 1;"
            ],
            Query
        ),
        once(query_result_from_file(
            Query,
            true,
            TmpFile
        )),
        read_rows(TmpFile, EventMaxId).

        %% event_table(-EventTable).
        event_table("publishers_list_collected_event").

%% select_clause(-SelectClause).
select_clause(SelectClause) :-
    append(
        [
            "SELECT ",
            "t.name::text AS string__name, ",
            "COALESCE(t.screen_name::text, ' ') AS string__username, ",
            "COALESCE(t.list_id::bigint, 0) AS number__list_id, ",
            "t.public_id::uuid AS string__public_id, ",
            "t.created_at AS string__created_at "
        ],
        SelectClause
    ).

%% by_list_uri_or_throw(+MainListAtUri, -Assoc).
by_list_uri_or_throw(list_uri(MainListAtUri), Assoc) :-
    ( ( by_list_uri(MainListAtUri, ListURIRows),
        writeln(rows: ListURIRows, true),
        nth0(0, ListURIRows, FirstListURIRow),
        get_assoc(payload, FirstListURIRow, FirstListURIRowPayload),
        get_assoc(list_id, FirstListURIRow, ListId),
        chars_base64(Utf8BytesPayload, FirstListURIRowPayload, []),
        maplist(char_code, Utf8BytesPayload, Utf8Bytes),
        chars_utf8bytes(PayloadChars, Utf8Bytes),
        append([Prefix, ['"']], PayloadChars),
        append([['"'], Suffix], Prefix),
        to_json_chars(Suffix, JSONChars),
        phrase(json_chars(pairs(Pairs)), JSONChars, []),
        write_term(pairs: Pairs, [quoted(false),double_quotes(true)]), nl,

        pairs_to_assoc(Pairs, AnonymousAssoc),
        writeln(anonymous_assoc:AnonymousAssoc, true),
        put_assoc(list_id, AnonymousAssoc, ListId, Assoc) )
    ->  true
    ;   throw(cannot_find_publishers_list_by_uri) ).

    %% by_list_uri(+ListURI, -HeadersAndRows).
    by_list_uri(ListURI, HeadersAndRows) :-
        chars_si(ListURI),
        from_event_table_clause(FromClause),
        append([
            "SELECT ",
            "payload as string__payload, ",
            "list_id as number__list_id, ",
            "list_name as string__list_name ",
            FromClause, " e ",
            "WHERE ",
            "e.occurred_at::date > '2024-12-31'::date ",
            "AND e.list_name = '", ListURI, "' ",
            "OFFSET 0 "
        ], SelectByListURI),
        append(
            [
                SelectByListURI,
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
                SelectByListURI,
                "LIMIT ALL;"
            ],
            SelectByListURIWithoutLimit
        ),
        writeln(by_list_URI:SelectByListURIWithoutLimit),

        once(query_result_from_file(
            SelectByListURIWithoutLimit,
            true,
            ByListURITmpFile
        )),
        (   read_rows(ByListURITmpFile, Rows)
        ->  true
        ;   throw(cannot_read_rows_selected_by(list_id)) ),

        maplist(to_json(Headers), Rows, Pairs),
        maplist(pairs_to_assoc, Pairs, HeadersAndRows).

%% from_clause(-FromClause).
from_clause(FromClause) :-
    table(Table),
    append(["FROM public.", Table, " t "], FromClause).

from_event_table_clause(FromClause) :-
    event_table(EventTable),
    append(["FROM public.", EventTable], FromClause).

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
count_matching_records(NextId, Result) :-
    event_table(EventTable),
    number_chars(NextId, NextIdChars),
    append([
        "SELECT count(*) how_many_records ",
        "FROM public.", EventTable, " ",
        "WHERE list_id::bigint = ", NextIdChars, ";"
    ], SelectQuery),
    once(query_result(
        SelectQuery,
        Result
    )).

%% insert(+Row, -InsertionResult).
insert(row(ListName, Payload), InsertionResult) :-
    next_id(NextId),

    count_matching_records(NextId, TotalMatchingRecords),

    if_(
        dif(1, TotalMatchingRecords),
        (uuidv4_string(NextPrimaryKey),
        number_chars(NextId, NextIdChars),
        event_table(EventTable),
        append(
            [
                "INSERT INTO public.", EventTable, " (",
                "   id,                 ",
                "   list_id,            ",
                "   list_name,          ",
                "   payload,            ",
                "   occurred_at,        ",
                "   started_at,         ",
                "   ended_at            ",
                ") VALUES (             ",
                "'", NextPrimaryKey, "',        ",
                "", NextIdChars, ", ",
                "'", ListName, "',      ",
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
        (event_table(EventTable),
        append(
            [
                "SELECT l.payload AS payload ",
                "FROM public.", EventTable," l ",
                "WHERE l.list_id::bigint = ", NextIdChars, ";"
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
