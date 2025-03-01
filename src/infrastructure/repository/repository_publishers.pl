:- module(repository_publishers, [
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

%% count(-Count).
count(Count) :-
    table(Table),
    append(
       ["SELECT count(*) AS Count FROM public.", Table],
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
            "t.id DESC, ",
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

%% by_list_uri(+ListURI, +DID, -HeadersAndRows).
by_criteria(list_uri(ListURI), did(DID), HeadersAndRows) :-
    chars_si(DID),
    select_clause(SelectClause),
    from_clause(FromClause),
    append([
        SelectClause,
        FromClause, " r ",
        "WHERE ",
        "r.created_at::date > '2024-12-31'::date ",
        "AND r.screen_name = '", DID, "' ",
        "AND r.name = '", ListURI, "' ",
        "OFFSET 0 "
    ], SelectByDID),
    append(
        [
            SelectByDID,
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
            SelectByDID,
            "LIMIT ALL;"
        ],
        SelectByDIDWithoutLimit
    ),
    writeln(selection_query(SelectByDIDWithoutLimit)),

    once(query_result_from_file(
        SelectByDIDWithoutLimit,
        true,
        ByDIDTmpFile
    )),
    (   read_rows(ByDIDTmpFile, Rows)
    ->  true
    ;   throw(cannot_read_rows_selected_by(list_id)) ),

    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

    %% from_clause(-FromClause).
    from_clause(FromClause) :-
        table(Table),
        append(["FROM public.", Table], FromClause).

    %% read_rows(+TmpFile, -Rows).
    read_rows(TmpFile, Rows) :-
        once(open(TmpFile, read, Stream, [type(text)])), !,
        (   file_exists(TmpFile)
        ->  true
        ;   write(file_does_not_exist), halt ),

        read_stream(Stream, StreamRows),
        once(phrase(rows(Rows), StreamRows, [])),
        remove_temporary_file(TmpFile).

    %% select_clause(-SelectClause).
    select_clause(SelectClause) :-
        append(
            [
                "SELECT ",
                "r.name as string__name, ",
                "r.screen_name as string__screen_name, ",
                "r.list_id as string__list_id, ",
                "r.public_id as string__public_id, ",
                "r.total_members as number__total_members, ",
                "r.total_statuses as number__total_statuses "
            ],
            SelectClause
        ).

%% insert(+Row, -InsertionResult).
insert(row(ListId, ListURI, _FollowersCount, _FollowsCount, DID, _), InsertionResult) :-
    count_matching_records(row(ListURI, DID), TotalMatchingRecords),

    if_(
        dif(1, TotalMatchingRecords),
       (next_id(NextId),
        number_chars(NextId, NextIdChars),
        uuidv4_string(PublicId),
        table(Table),
        append(
            [
                "INSERT INTO public.", Table, " (",
                "   id,                 ",
                "   list_id,            ",
                "   public_id,          ",
                "   name,               ",
                "   screen_name,        ",
                "   locked,             ",
                "   created_at          ",
                ") VALUES (             ",
                "'", NextIdChars, "',   ",
                " ", ListId, ",         ",
                "'", PublicId, "',      ",
                "'", ListURI, "',       ",
                "'", DID, "',           ",
                "   false,              ",
                "   NOW()               ",
                ")"
            ],
            Query
        ),
        once(query_result(
            Query,
            InsertionResult
        ))),
        (table(Table),
        append(
            [
                "SELECT                             ",
                "r.id AS number__id,                ",
                "r.public_id AS string__public_id,  ",
                "r.name AS string__name             ",
                "FROM public.", Table, " r     ",
                "WHERE                              ",
                "r.screen_name = '", DID, "'        ",
                "AND r.name = '", ListURI, "'      ;"
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

    %%% count_matching_records(+Row, -Result).
    count_matching_records(row(ListURI, DID), Result) :-
        table(Table),
        append([
            "SELECT count(*) how_many_records ",
            "FROM public.", Table, " r ",
            "WHERE name = '", ListURI, "' ",
            "AND screen_name = '", DID, "';"
        ], SelectQuery),
        once(query_result(
            SelectQuery,
            Result
        )).
