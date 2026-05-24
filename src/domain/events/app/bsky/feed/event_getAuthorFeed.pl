:- module(event_getAuthorFeed, [
    onGetAuthorFeed/4,
    insert_record_args/9
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
    exists_by_uri_t/3,
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
    catch(
        onGetAuthorFeed(Cursor, Post),
        pg_query_silently_failed(SQL, Params),
        ( record_skipped_post(Index, Post, pg_query_silently_failed(SQL, Params)),
          writeln(
              [skipped_post|[
                  index(Index/TotalPosts),
                  reason(pg_query_silently_failed)
              ]],
              true
          )
        )
    ).

    %% Handling onGetAuthorFeed Event
    %
    %% onGetAuthorFeed(+Cursor, +Post)
    %
    % Cursor is the page-level NextCursor, kept in the signature
    % for the maplist call shape but no longer used for dedup --
    % per-post existence is now keyed on the post's own URI via
    % the ust_hash unique index. Keying on the page cursor wrongly
    % rejected every post on a page whose oldest indexedAt second
    % collided with an existing row.
    onGetAuthorFeed(_Cursor, Post) :-
        insert_record_args(
            Post,
            DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
            likes(LikeCount)-reposts(RepostCount)
        ),
        if_(
            exists_by_uri_t(Handle, URI),
            throw(already_indexed_post(uri(URI))),
            ( writeln([no_records_found_by_uri|[URI]], true),
              try_inserting_publication_record(
                  DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
                  likes(LikeCount)-reposts(RepostCount),
                  _PublicationInsertionResult
              )
            )
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

%% record_skipped_post(+Index, +Post, +Reason).
%
% Append the offending Post term and surrounding context to
% /tmp/segv-investigation/skipped_posts.pl when onGetAuthorFeed/2
% surfaces a labelled pg_query_silently_failed/2 throw and the
% outer maplist routes around it. The file is the same convention
% used by capture_feed_response/2 in getAuthorFeed.pl and
% record_pg_query_failure/3 in client.pl, so a single sweep of
% /tmp/segv-investigation/ recovers every artifact tied to a
% given run. I/O errors are swallowed: this writer must never
% mask the underlying pg_query failure it is documenting.
%
% Each entry is one self-contained clause:
%
%   skipped_post(
%     at(<ISO-8601 chars>),
%     index(<integer>),
%     reason(<Reason>),
%     post(<full JSON-DCG pair term>)
%   ).
%
% so a downstream reader can `consult/1` the file and replay each
% skipped post in isolation.
record_skipped_post(Index, Post, Reason) :-
    catch(
        ( current_time(T),
          phrase(format_time("%Y-%m-%dT%H:%M:%SZ", T), TimeChars),
          open("/tmp/segv-investigation/skipped_posts.pl",
               append, Stream, [type(text)]),
          write_canonical(Stream,
              skipped_post(
                  at(TimeChars),
                  index(Index),
                  reason(Reason),
                  post(Post)
              )),
          write(Stream, '.'),
          nl(Stream),
          close(Stream)
        ),
        _,
        true
    ).
