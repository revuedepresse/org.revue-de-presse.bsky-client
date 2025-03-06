:- module(event_getAuthorFeed, [
    onGetAuthorFeed/4
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(serialization/json)).
:- use_module(library(time)).

:- use_module('../../../../../infrastructure/repository/repository_status', [
    by_criteria/2,
    by_indexed_at/2,
    id_hash/3,
    insert/2
]).
:- use_module('../../../../../infrastructure/repository/repository_popularity', [
    insert_without_unicity_check/2
]).
:- use_module('../../../../../infrastructure/repository/repository_publication', [
    by_criteria/2,
    insert/2
]).
:- use_module('../../../../../logger', [
    log_debug/1,
    log_error/1,
    log_info/1
]).
:- use_module('../../../../../serialization', [
    char_code_at/2,
    pairs_to_assoc/2,
    to_json_chars/2,
    wrapped_pairs_to_assoc/2
]).
:- use_module('../../../../../stream', [
    writeln/1,
    writeln/2
]).

%% onGetAuthorFeed(+Cursor, +TotalPosts, +Post, +Index)
onGetAuthorFeed(Cursor, TotalPosts, Post, Index) :-
    writeln([processing_post_at_index|[(Index/TotalPosts)]], true),
    onGetAuthorFeed(Cursor, Post).

    %% Handling onGetAuthorFeed Event
    %
    %% onGetAuthorFeed(+Cursor, +Post)
    onGetAuthorFeed(Cursor, Post) :-
        insert_record_args(
            Post,
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            likes(LikeCount)-reposts(RepostCount)
        ),
        by_indexed_at(indexed_at(Cursor)-handle(Handle), Rows),
        length(Rows, N),
        if_(
            N = 0,
            writeln([no_records_found_by_cursor|[Cursor]], true),
            throw(already_indexed_post(uri(URI)))
        ),

        try_inserting_publication_record(
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            likes(LikeCount)-reposts(RepostCount),
            _PublicationInsertionResult
        ).

        %% insert_record_args(+Post, -DisplayName, -Handle, -Text, -AuthorAvatar, -Payload, -URI, -CreatedAt, -Popularity)
        insert_record_args(
            Post,
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            likes(LikeCount)-reposts(RepostCount)
        ) :-
            wrapped_pairs_to_assoc(Post, PostAssoc),
            writeln(post:Post),
            writeln(assoc:PostAssoc),

            get_assoc(post, PostAssoc, FeedPost),
            writeln(feed_post:FeedPost),

            get_assoc(author, FeedPost, Author),
            get_assoc(displayName, Author, DisplayName),
            get_assoc(handle, Author, Handle),
            get_assoc(avatar, Author, AuthorAvatar),

            get_assoc(likeCount, FeedPost, LikeCount),
            get_assoc(repostCount, FeedPost, RepostCount),

            get_assoc(record, FeedPost, Record),
            get_assoc(text, Record, Text),
            get_assoc(createdAt, Record, CreatedAt),

            get_assoc(uri, FeedPost, URI),
            Payload = FeedPost.

        %% try_inserting_publication_record(
        %%  +DisplayName, +Handle, +Text, +AuthorAvatar, +Payload, +URI, +CreatedAt,
        %%  +Popularity,
        %%  -InsertionResult
        %% ),
        try_inserting_publication_record(
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            likes(LikeCount)-reposts(RepostCount),
            InsertionResult
        ) :-
            try_inserting_status_record(
                DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
                likes(LikeCount)-reposts(RepostCount),
                RecordId
            ),
            catch(
                (once(repository_publication:by_criteria(handle(Handle)-uri(URI), Rows)),
                ( ( nth0(0, Rows, FirstRow),
                    get_assoc(handle, FirstRow, PreExistingPostHandle) )
                ->  writeln(['Pre-existing publication for handle'|[PreExistingPostHandle]], true)
                ;   writeln(['No pre-existing publication for handle'|[Handle]], true),
                    throw(cannot_read_rows_selection))),
                Cause,
                if_(
                    Cause = cannot_read_rows_selection,
                   (repository_publication:insert(
                        row(
                            DisplayName,
                            Handle,
                            Text,
                            AuthorAvatar,
                            Payload,
                            URI,
                            CreatedAt,
                            RecordId
                        ),
                        InsertionResult
                    ),
                    writeln(publication_record_insertion(InsertionResult), true)),
                   (writeln(could_not_insert_publication_with_uri(URI), true),
                    throw(pre_existing_author_feed_post(Cause)))
                )
            ).

        %% try_inserting_status_record(+DisplayName, +Handle, +Text, +AuthorAvatar, +Payload, +URI, +CreatedAt, +Popularity, -RecordId),
        try_inserting_status_record(
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            likes(LikeCount)-reposts(RepostCount),
            RecordId
        ) :-
            writeln([likes_reposts|[likes(LikeCount)-reposts(RepostCount)]], true),
            if_(
                repository_status:id_hash(handle(Handle)-uri(URI), RecordId),
               (repository_popularity:insert_without_unicity_check(
                    row(RecordId, URI, LikeCount, RepostCount),
                    _PopularityInsertionResult
                )),
                catch(
                    (once(repository_status:by_criteria(handle(Handle)-uri(URI), Rows)),
                    ( ( nth0(0, Rows, FirstRow),
                        get_assoc(handle, FirstRow, PreExistingPostHandle) )
                    ->  writeln([pre_existing_status_for_handle|[PreExistingPostHandle]], true)
                    ;   throw(cannot_read_rows_selection) )),
                    Cause,
                    if_(
                        Cause = cannot_read_rows_selection,
                       (repository_status:insert(
                            row(
                                DisplayName,
                                Handle,
                                Text,
                                AuthorAvatar,
                                Payload,
                                URI,
                                CreatedAt
                            ),
                            InsertionResult,
                            RecordId
                        ),
                        once(( ground(RecordId) ; throw(invalid_publication_id) )),
                        writeln([inserted_record_id|[RecordId]], true),
                        repository_popularity:insert_without_unicity_check(
                            row(RecordId, URI, LikeCount, RepostCount),
                            _PopularityInsertionResult
                        ),
                        writeln([status_record_insertion|[InsertionResult]], true)),
                       (writeln(could_not_insert_status_with_uri(URI), true),
                        throw(pre_existing_author_feed_post(Cause)))
                    )
                )
            ).
