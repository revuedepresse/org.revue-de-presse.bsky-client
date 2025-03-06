:- module(repository_status, [
    by_criteria/2,
    by_indexed_at/2,
    count/1,
    id_hash/3,
    insert/3,
    next_id/1,
    query/1
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(crypto)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(files)).
:- use_module(library(lists)).
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
    hash/2,
    matching_criteria/2,
    query_result/2,
    query_result_from_file/3,
    read_rows/2
]).
:- use_module('../../logger', [
    log_debug/1,
    log_error/1,
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
       ["SELECT count(*) AS matching_records_count FROM public.", Table],
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
            "ust_id DESC ",
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
                "SELECT COALESCE(r.ust_id::bigint, 0) pk ",
                "FROM ", Table, " r ",
                "ORDER BY r.ust_id DESC ",
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

%% by_criteria(+Criteria, -HeadersAndRows).
by_criteria(handle(Handle)-uri(URI), HeadersAndRows) :-
    catch(
        once((
            chars_si(Handle),
            chars_si(URI)
        )),
        E,
        log_error([invalid_unique_identifier(E)])
    ),
    hash(handle(Handle)-uri(URI), Hash),
    select_clause(SelectClause),
    from_clause(FromClause),
    append([
        SelectClause,
        FromClause,
        "WHERE ",
        "r.ust_hash = '", Hash, "' ",
        "OFFSET 0 "
    ], SelectByCriteria),
    matching_criteria(SelectByCriteria, HeadersAndRows).

    %% from_clause(-FromClause).
    from_clause(FromClause) :-
        table(Table),
        append(["FROM public.", Table, " r "], FromClause).

    %% select_clause(-SelectClause).
    select_clause(SelectClause) :-
        append(
            [
                "SELECT                                             ",
                "r.ust_id::bigint AS number__id,                    ",
                "r.ust_full_name AS string__full_name,              ",
                "r.ust_avatar AS string__avatar,                    ",
                "r.ust_status_id AS string__status_id,              ",
                "to_json(r.is_published) AS boolean__is_published   "
            ],
            SelectClause
        ).

%% insert(+Row, -InsertionResult, NextIdChars).
insert(
    row(
        _FullName,
        Handle,
        PreQuotingText,
        Avatar,
        PreEncodingPayload,
        URI,
        CreatedAt
    ),
    InsertionResult,
    InsertedRecordId
) :-
    hash(handle(Handle)-uri(URI), Hash),

    count_matching_records(row(Hash), TotalMatchingRecords),
    writeln([total_matching_records|[TotalMatchingRecords]], true),

    encode_field_value(PreQuotingText, Text),
    encode_field_value(PreEncodingPayload, Payload),

    if_(
        TotalMatchingRecords = 0,
       (table(Table),
        append(
            [
                "INSERT INTO public.", Table, " (",
                "   ust_hash,           ",
                "   ust_name,           ",
                "   ust_full_name,      ",
                "   ust_text,           ",
                "   ust_avatar,         ",
                "   ust_api_document,   ",
                "   ust_status_id,      ",
                "   ust_access_token,   ",
                "   is_published,       ",
                "   ust_created_at      ",
                ") VALUES (             ",
                "'", Hash, "',          ",
                "'", Handle, "',        ",
                "'", Handle, "',        ",
                "'", Text, "',          ",
                "'", Avatar, "',        ",
                "'", Payload, "',       ",
                "'", URI, "',           ",
                "'dummy_access_token',  ",
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
        InsertionResult = ok
    ),

    by_hash(Hash, Assoc),
    get_assoc(id, Assoc, InsertedRecordId).

    %% id_hash(+UniqueIdentifier, -RecordId, -Result).
    id_hash(UniqueIdentifier, RecordId, Result) :-
        (   catch(
                once((
                    catch(chars_si(UniqueIdentifier), E, throw(invalid_unique_identifier(E))),
                    hash(UniqueIdentifier, Hash),
                    by_hash(Hash, Assoc),
                    get_assoc(id, Assoc, RecordId),
                    Result = true,
                    writeln([found_record__id|[RecordId]], true)
                )),
                CannotSelectRecordByHashCause,
                if_(
                    CannotSelectRecordByHashCause = no_records_found,
                    fail,
                    throw(cannot_select_record_by_hash(CannotSelectRecordByHashCause))
                )
            )
        ; ( Result = false,
            writeln([no_record_found_by_unique_identifier|[UniqueIdentifier]], true) )
        ).

    %% by_hash(+Hash, -Assoc).
    by_hash(Hash, Assoc) :-
        table(Table),
        append(
            [
                "SELECT                                     ",
                "r.ust_id as number__id,                    ",
                "r.ust_name as string__name,                ",
                "r.ust_hash as string__hash,                ",
                "r.ust_full_name as string__full_name,      ",
                "r.ust_text as string__text,                ",
                "r.ust_avatar as string__avatar,            ",
                "r.ust_status_id as string__status_id,      ",
                "r.is_published as boolean__is_published,   ",
                "r.ust_created_at as string__created_at     ",
                "FROM public.", Table, " r                  ",
                "WHERE                                      ",
                "r.ust_hash = '", Hash, "'                  ",
                "OFFSET 0                                   "
            ],
            SelectRecordQueryByHash
        ),
        once(query_result(
            SelectRecordQueryByHash,
            SelectionResult
        )),

        if_(
            SelectionResult = no_records_found,
            throw(SelectionResult),
           (writeln([selection_result_by_hash|[SelectionResult]]),
            matching_criteria(SelectRecordQueryByHash, Rows),
            nth0(0, Rows, Assoc))
        ).

    %%% count_matching_records(+Row, -Result).
    count_matching_records(row(Hash), Result) :-
        table(Table),
        append([
            "SELECT count(*) how_many_records   ",
            "FROM public.", Table, " r          ",
            "WHERE ust_hash = '", Hash, "'     ;"
        ], SelectQuery),
        once(query_result(
            SelectQuery,
            Result
        )).

%% by_indexed_at(+Criteria, -HeadersAndRows).
by_indexed_at(indexed_at(IndexedAt)-handle(Handle), HeadersAndRows) :-
    select_clause(SelectClause),
    from_clause(FromClause),

    length(Prefix, 10),
    length(Suffix, 8),
    append([Prefix, [_], Suffix, _Rest], IndexedAt),
    append([Prefix, " ", Suffix], IndexedAtDate),

    append([
        SelectClause,
        FromClause,
        "WHERE ",
        "r.ust_name = '", Handle, "' ",
        "AND r.ust_created_at::timestamp = '", IndexedAtDate, "'::timestamp ",
        "OFFSET 0 "
    ], SelectByCriteria),
    matching_criteria(SelectByCriteria, HeadersAndRows).

%% table(-Table)
table("weaving_status").
