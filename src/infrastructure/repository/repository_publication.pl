:- module(repository_publication, [
    by_criteria/2,
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
    encode_field_value/2,
    hash/2,
    matching_criteria/2,
    query_result/2,
    query_result_from_file/3,
    read_rows/2
]).
:- use_module('../pg/connection', [pg_query/3]).
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

%% by_criteria(+Criteria, -HeadersAndRows).
by_criteria(handle(Handle)-uri(URI), HeadersAndRows) :-
    hash(handle(Handle)-uri(URI), Hash),
    catch(
        once((
            chars_si(Handle),
            chars_si(URI)
        )),
        CannotSelectByCriteria,
        throw(invalid_unique_identifier(CannotSelectByCriteria))
    ),
    select_clause(SelectClause),
    from_clause(FromClause),
    append([
        SelectClause,
        FromClause, " ",
        "WHERE ",
        "r.hash = '", Hash, "' ",
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
                "r.legacy_id    AS number__id,          ",
                "r.screen_name  AS string__full_name,   ",
                "r.avatar_url   AS string__avatar,      ",
                "r.document_id  AS string__status_id    "
            ],
            SelectClause
        ).

%% insert(+Row, -InsertionResult).
%
% Bind-parameter INSERT through the wire client. Idempotent at the DB
% layer via ON CONFLICT (hash) DO NOTHING; legacy_id is RETURNED on
% successful insert, empty rows on duplicate.
%
% InsertionResult is one of:
%   - new(LegacyIdChars)    a row was inserted, RETURNING gave us its legacy_id.
%   - duplicate(Hash)       the hash already existed; no row added.
insert(
    row(
        _FullName,
        Handle,
        PreQuotingText,
        Avatar,
        PreEncodingPayload,
        URI,
        CreatedAt,
        RecordId
    ),
    InsertionResult
) :-
    hash(handle(Handle)-uri(URI), Hash),
    uuidv4_string(PublicId),
    encode_field_value(PreEncodingPayload, Payload),
    encode_field_value(PreQuotingText, Text),
    publication_insert_sql(SQL),
    Params = [PublicId, RecordId, Hash, Handle, Text, Avatar, URI, Payload, CreatedAt],
    pg_query(SQL, Params, Reply),
    interpret_publication_insert(Reply, Hash, InsertionResult).

publication_insert_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " (",
            "id, legacy_id, hash, screen_name, text, avatar_url, document_id, document, published_at",
            ") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9) ",
            "ON CONFLICT (hash) DO NOTHING ",
            "RETURNING legacy_id"
        ],
        SQL
    ).

interpret_publication_insert(data([[LegacyIdChars|_]|_]), _Hash, new(LegacyIdChars)) :-
    !, writeln([publication_inserted|[LegacyIdChars]], true).
interpret_publication_insert(data([]), Hash, duplicate(Hash)) :-
    !, writeln([publication_duplicate_skipped|[Hash]], true).
interpret_publication_insert(error(Err), _, _) :-
    throw(pg_error(Err)).

    %%% count_matching_records(+Row, -Result).
    count_matching_records(row(Hash), Result) :-
        table(Table),
        append([
            "SELECT COUNT(*) how_many_records   ",
            "FROM public.", Table, " r          ",
            "WHERE r.hash = '", Hash, "'       ;"
        ], SelectQuery),
        once(query_result(
            SelectQuery,
            Result
        )).

    %% table(-Table)
    table("publication").
