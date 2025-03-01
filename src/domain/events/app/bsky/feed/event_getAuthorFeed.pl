:- module(event_getAuthorFeed, [
    onGetAuthorFeed/4
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(serialization/json)).
:- use_module(library(time)).

:- use_module('../../../../../infrastructure/repository/repository_statuses', [
    by_criteria/2,
    by_indexed_at/2,
    insert/2
]).
:- use_module('../../../../../infrastructure/repository/repository_publications', [
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
    writeln(processing_post_at_index(Index/TotalPosts), true),
    onGetAuthorFeed(Cursor, Post).

    %% Handling onGetAuthorFeed Event
    %
    %% onGetAuthorFeed(+Post)
    onGetAuthorFeed(Cursor, Post) :-
        insert_record_args(
            Post,
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt
        ),
        by_indexed_at(indexed_at(Cursor)-handle(Handle), Rows),
        length(Rows, N),
        if_(
            N = 0,
            writeln([found_no_pre_existing_post_at_cursor|[Cursor]], true),
            throw(already_indexed_post(uri(URI)))
        ),

        try_inserting_publication_record(
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            _PublicationInsertionResult
        ),

        try_inserting_status_record(
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            _StatusInsertionResult
        ).

        insert_record_args(
            Post,
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt
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
            writeln(likes(LikeCount)-reposts(RepostCount), true),

            get_assoc(record, FeedPost, Record),
            get_assoc(text, Record, Text),
            get_assoc(createdAt, Record, CreatedAt),

            get_assoc(uri, FeedPost, URI),
            Payload = FeedPost.

        %% try_inserting_publication_record(+DisplayName, +Handle, +Text, +AuthorAvatar, +Payload, +URI, +CreatedAt, -InsertionResult),
        try_inserting_publication_record(
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            InsertionResult
        ) :-
            catch(
                (once(repository_publications:by_criteria(handle(Handle)-uri(URI), Rows)),
                ( ( nth0(0, Rows, FirstRow),
                    get_assoc(handle, FirstRow, PreExistingPostHandle) )
                ->  writeln(['Pre-existing publication for handle'|[PreExistingPostHandle]], true)
                ;   writeln(['No pre-existing publication for handle'|[Handle]], true),
                    throw(cannot_read_rows_selection))),
                Cause,
                if_(
                    Cause = cannot_read_rows_selection,
                   (repository_publications:insert(
                        row(
                            DisplayName,
                            Handle,
                            Text,
                            AuthorAvatar,
                            Payload,
                            URI,
                            CreatedAt
                        ),
                        InsertionResult
                    ),
                    writeln(publication_record_insertion(InsertionResult), true)),
                   (writeln(could_not_insert_publication_with_uri(URI), true),
                    throw(pre_existing_author_feed_post(Cause)))
                )
            ).

        %% try_inserting_status_record(+DisplayName, +Handle, +Text, +AuthorAvatar, +Payload, +URI, +CreatedAt, -InsertionResult),
        try_inserting_status_record(
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            InsertionResult
        ) :-
            catch(
                (once(repository_statuses:by_criteria(handle(Handle)-uri(URI), Rows)),
                ( ( nth0(0, Rows, FirstRow),
                    get_assoc(handle, FirstRow, PreExistingPostHandle) )
                ->  writeln('Pre-existing status for handle':PreExistingPostHandle, true)
                ;   true )),
                Cause,
                if_(
                    Cause = cannot_read_rows_selection,
                   (repository_statuses:insert(
                        row(
                            DisplayName,
                            Handle,
                            Text,
                            AuthorAvatar,
                            Payload,
                            URI,
                            CreatedAt
                        ),
                        InsertionResult
                    ),
                    writeln(status_record_insertion(InsertionResult), true)),
                   (writeln(could_not_insert_status_with_uri(URI), true),
                    throw(pre_existing_author_feed_post(Cause)))
                )
            ).
