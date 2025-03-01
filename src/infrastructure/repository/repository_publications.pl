:- module(repository_publications, [
    by_criteria/2,
    count/1,
    insert/2,
    next_id/1,
    query/1
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(crypto)).
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
    encode_field_value/2,
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
    query(Rows, true, 2),
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
            "legacy_id DESC ",
            "OFFSET 0 ",
            "LIMIT ", LimitClause, ";"
        ],
        Query
    ).

%% query(-Rows, +TuplesOnly, +Limit).
query(Rows, TuplesOnly, Limit) :-
    all_clauses(Query, Limit),
    writeln(query:Query, true),
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
                "SELECT COALESCE(r.legacy_id::bigint, 0) pk ",
                "FROM ", Table, " r ",
                "ORDER BY r.legacy_id DESC ",
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
        table("publication").

%% by_list_uri(+Criteria, -HeadersAndRows).
by_criteria(handle-(Handle)-uri(URI), HeadersAndRows) :-
    append([Handle, "|", URI], UniqueIdentifier),
    crypto_data_hash(UniqueIdentifier, Hash, [algorithm(sha256)]),

    chars_si(URI),
    select_clause(SelectClause),
    from_clause(FromClause),
    append([
        SelectClause,
        FromClause, " ",
        "WHERE ",
        "r.hash = '", Hash, "' ",
        "OFFSET 0 "
    ], SelectByURI),
    append(
        [
            SelectByURI,
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
            SelectByURI,
            "LIMIT ALL;"
        ],
        SelectByURIWithoutLimit
    ),
    writeln(selection_query(SelectByURIWithoutLimit)),

    once(query_result_from_file(
        SelectByURIWithoutLimit,
        true,
        ByDIDTmpFile
    )),
    (   read_rows(ByDIDTmpFile, Rows)
    ->  true
    ;   throw(cannot_read_rows_selected_by(status_id)) ),

    maplist(to_json(Headers), Rows, Pairs),
    maplist(pairs_to_assoc, Pairs, HeadersAndRows).

    %% from_clause(-FromClause).
    from_clause(FromClause) :-
        table(Table),
        append(["FROM public.", Table, " r "], FromClause).

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
                "r.legacy_id::bigint as number__id, ",
                "r.screen_name as string__full_name, ",
                "r.avatar_url as string__avatar, ",
                "r.document_id as string__status_id "
            ],
            SelectClause
        ).

%% insert(+Row, -InsertionResult).
insert(
    row(
        _FullName,
        Name,
        PreQuotingText,
        Avatar,
        PreEncodingPayload,
        URI,
        CreatedAt
    ),
    InsertionResult
) :-
    append([Name, "|", URI], UniqueIdentifier),
    crypto_data_hash(UniqueIdentifier, Hash, [algorithm(sha256)]),

    count_matching_records(row(Hash), TotalMatchingRecords),
    encode_field_value(PreEncodingPayload, Payload),
    encode_field_value(PreQuotingText, Text),

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
                "   legacy_id,          ",
                "   hash,               ",
                "   screen_name,        ",
                "   text,               ",
                "   avatar_url,         ",
                "   document_id,        ",
                "   document,           ",
                "   published_at        ",
                ") VALUES (             ",
                "'", PublicId, "',      ",
                "'", NextIdChars, "',   ",
                "'", Hash, "',          ",
                "'", Name, "',          ",
                "'", Text, "',          ",
                "'", Avatar, "',        ",
                "'", URI, "',           ",
                "'", Payload, "',       ",
                "   true,               ",
                "'", CreatedAt, "'      ",
                ");"
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
                "SELECT                                     ",
                "r.screen_name as string__name,             ",
                "r.hash as string__hash,                    ",
                "r.text as string__text,                    ",
                "r.avatar_url as string__avatar,            ",
                "r.document_id as string__status_id,        ",
                "r.published_at as string__created_at       ",
                "FROM public.", Table, "                    ",
                "WHERE                                      ",
                "r.hash = '", Hash, "'               ;"
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
    count_matching_records(row(URI), Result) :-
        table(Table),
        append([
            "SELECT COUNT(*) how_many_records ",
            "FROM public.", Table, " r ",
            "WHERE document_id = '", URI, "' ;"
        ], SelectQuery),
        once(query_result(
            SelectQuery,
            Result
        )).
