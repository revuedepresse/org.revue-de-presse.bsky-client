:- module(event_getAuthorFeed, [
    onGetAuthorFeed/4
]).

/**
Domain event handler for `app.bsky.feed.getAuthorFeed`.

Called once per post returned by the feed traversal. Decodes
each post into the fields the repository layer expects,
short-circuits if a status row already exists at this cursor,
then chains three idempotent writes: `weaving_status` (with
ON CONFLICT to recover an existing `ust_id`), `publication`
(also with ON CONFLICT), and `status_popularity` (append-only).
Idempotence is enforced at the database, not in prolog, so a
re-run of the worker leaves no duplicates.
*/

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
%
% Handle one post returned by `getAuthorFeed`. `Cursor` is the
% feed cursor's ISO-8601 timestamp; `Post` is the JSON-DCG
% pair list for the post; `Index` and `TotalPosts` carry the
% post's position on the current page for logging. Writes
% `weaving_status`, `publication`, and `status_popularity` in
% sequence, each idempotent at the DB layer.
onGetAuthorFeed(Cursor, TotalPosts, Post, Index) :-
    writeln([processing_post_at_index|[(Index/TotalPosts)]], true),
    onGetAuthorFeed(Cursor, Post).

    %% Handling onGetAuthorFeed Event
    %
    %% onGetAuthorFeed(+Cursor, +Post)
    onGetAuthorFeed(Cursor, Post) :-
        writeln([step|insert_record_args], true),
        insert_record_args(
            Post,
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            likes(LikeCount)-reposts(RepostCount)
        ),
        writeln([step|by_indexed_at], true),
        by_indexed_at(indexed_at(Cursor)-handle(Handle), Rows),
        length(Rows, N),
        writeln([step|check_already_indexed], true),
        if_(
            N = 0,
            writeln([no_records_found_by_cursor|[Cursor]], true),
            throw(already_indexed_post(uri(URI)))
        ),

        writeln([step|try_inserting_publication_record], true),
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
        %
        % Idempotence is enforced at the DB layer via UNIQUE (hash);
        % repository_publication:insert/2 uses INSERT ... ON CONFLICT DO
        % NOTHING RETURNING legacy_id and tells us whether a row was
        % actually written (new(_)) or skipped (duplicate(_)).
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
            repository_publication:insert(
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
            writeln(publication_record_insertion(InsertionResult), true).

        %% try_inserting_status_record(+DisplayName, +Handle, +Text, +AuthorAvatar, +Payload, +URI, +CreatedAt, +Popularity, -RecordId),
        %
        % repository_status:insert/3 is now idempotent at the DB layer
        % (UNIQUE (ust_hash) + ON CONFLICT DO NOTHING RETURNING ust_id),
        % and always yields a ust_id - either freshly minted or fetched
        % via the post-conflict SELECT. Popularity is always appended.
        try_inserting_status_record(
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            likes(LikeCount)-reposts(RepostCount),
            RecordId
        ) :-
            writeln([likes_reposts|[likes(LikeCount)-reposts(RepostCount)]], true),
            repository_status:insert(
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
            writeln([status_record_insertion|[InsertionResult]], true),
            writeln([inserted_record_id|[RecordId]], true),
            repository_popularity:insert_without_unicity_check(
                row(RecordId, URI, LikeCount, RepostCount),
                _PopularityInsertionResult
            ).
