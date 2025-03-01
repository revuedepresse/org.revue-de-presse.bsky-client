:- module(event_getAuthorFeed, [
    onGetAuthorFeed/1,
    onGetAuthorFeed/2
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(serialization/json)).
:- use_module(library(time)).

:- use_module('../../../../../infrastructure/repository/repository_statuses', [
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

%% onGetAuthorFeed(+Post, +Index)
onGetAuthorFeed(Post, Index) :-
    writeln(processing_post_at_index(Index), true),
    onGetAuthorFeed(Post).

%% Handling onGetAuthorFeed Event
%
%% onGetAuthorFeed(+Post)
onGetAuthorFeed(Post) :-
    wrapped_pairs_to_assoc(Post, PostAssoc),
    writeln(post:Post),
    writeln(assoc:PostAssoc),

    get_assoc(post, PostAssoc, FeedPost),
    writeln(feed_post:FeedPost),

    get_assoc(author, FeedPost, Author),
    get_assoc(displayName, Author, DisplayName),
    get_assoc(handle, Author, Handle),
    get_assoc(avatar, Author, AuthorAvatar),

    get_assoc(record, FeedPost, Record),
    get_assoc(text, Record, Text),
    get_assoc(createdAt, Record, CreatedAt),

    get_assoc(uri, FeedPost, URI),

    Payload = FeedPost,

    catch(
        (once(repository_statuses:by_criteria(handle(Handle)-uri(URI), Rows)),
        ( ( nth0(0, Rows, FirstRow),
            get_assoc(handle, FirstRow, PreExistingPostHandle) )
        ->  log_info(['Pre-existing record for handle':PreExistingPostHandle])
        ;   true )),
        Cause,
        if_(
            Cause = cannot_read_rows_selected_by(status_id),
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
            writeln(record_insertion(InsertionResult), true)),
           (writeln(could_not_insert_status_with_uri(URI), true),
            throw(pre_existing_author_feed_post(Cause)))
        )
    ).

%get_key(string(Key)-_Value, Key).
