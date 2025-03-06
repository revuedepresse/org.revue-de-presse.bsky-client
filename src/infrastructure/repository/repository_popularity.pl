:- module(repository_popularity, [
    by_criteria/2,
    count/1,
    insert/2,
    insert_without_unicity_check/2,
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
    encode_field_value/2,
    matching_criteria/2,
    query_result/2,
    query_result_from_file/3,
    read_rows/2
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
       ["SELECT COUNT(*) AS matching_records_count FROM public.", Table],
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
            "status_id DESC ",
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
                "SELECT COALESCE(r.status_id::bigint, 0) pk ",
                "FROM ", Table, " r ",
                "ORDER BY r.status_id DESC ",
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
by_criteria(record_id(RecordId), HeadersAndRows) :-
    chars_si(RecordId),
    select_clause(SelectClause),
    from_clause(FromClause),
    append([
        SelectClause,
        FromClause, " ",
        "WHERE ",
        "r.publication_id = '", RecordId, "' ",
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
                "SELECT ",
                "r.id as string__id,                   ",
                "r.publication_id as string__id,       ",
                "r.total_favorites as number__likes,   ",
                "r.total_retweets as number__reposts,  ",
                "r.checked_at as number__string        "
            ],
            SelectClause
        ).

%% insert(+Row, -InsertionResult).
insert(
    row(
        RecordId,
        URI,
        Likes,
        RePosts
    ),
    InsertionResult
) :-
    count_matching_records(row(URI), TotalMatchingRecords),

    if_(
        dif(1, TotalMatchingRecords),
       (uuidv4_string(Id),
        table(Table),
        append(
            [
                "INSERT INTO public.", Table, " (",
                "   id,                 ",
                "   status_id,          ",
                "   publication_id,     ",
                "   total_favorites,    ",
                "   total_retweets,     ",
                "   checked_at          ",
                ") VALUES (             ",
                "'", Id, "',            ",
                "'", RecordId, "', ",
                "'", URI, "', ",
                "'", Likes, "',         ",
                "'", RePosts, "',       ",
                "NOW()                  ",
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
                "SELECT                                 ",
                "r.publication_id as string__id,        ",
                "r.total_favorites as number__likes,    ",
                "r.total_retweets as number__reposts    ",
                "r.checked_at as number__string         ",
                "FROM public.", Table, " r              ",
                "WHERE                                  ",
                "r.publication_id = '", URI, "'        ;"
            ],
            SelectQuery
        ),
        matching_criteria(SelectQuery, Rows),
        writeln([popularity_rows|[Rows]], true),
        InsertionResult = ok)
    ).

%% insert_without_unicity_check(+Row, -InsertionResult).
insert_without_unicity_check(Row, InsertionResult) :-
    catch(
        do_insert_without_unicity_check(
            Row,
            InsertionResult
        ),
        CannotInsertPopularityWithoutUnicityCheckCause,
        log_error([cannot_insert_popularity_without_unicity_check(CannotInsertPopularityWithoutUnicityCheckCause)])
    )
    ->  writeln([inserted_popularity_without_unicity_check_successfully|[]], true)
    ;   writeln([cannot_insert_popularity_without_unicity_check(Row)], true).

%% do_insert_without_unicity_check(+Row, -InsertionResult).
do_insert_without_unicity_check(
    row(
        RecordId,
        URI,
        Likes,
        RePosts
    ),
    InsertionResult
) :-
    table(Table),
    uuidv4_string(Id),
    number_chars(Likes, LikesChars),
    number_chars(RePosts, RePostsChars),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "   id,                 ",
            "   status_id,          ",
            "   publication_id,     ",
            "   total_favorites,    ",
            "   total_retweets,     ",
            "   checked_at          ",
            ") VALUES (             ",
            "'", Id, "',            ",
            "'", RecordId, "',      ",
            "'", URI, "',           ",
            "'", LikesChars, "',    ",
            "'", RePostsChars, "',  ",
            "NOW()                  ",
            ");"
        ],
        Query
    ),
    writeln([insert_popularity_query|[Query]]),
    once(query_result(
        Query,
        InsertionResult
    )).

    %%% count_matching_records(+Row, -Result).
    count_matching_records(row(URI), Result) :-
        table(Table),
        append([
            "SELECT COUNT(*) how_many_records ",
            "FROM public.", Table, " r      ",
            "WHERE r.publication_id = '", URI, "'   ;"
        ], SelectQuery),
        once(query_result(
            SelectQuery,
            Result
        )).

    %% table(-Table)
    table("status_popularity").
