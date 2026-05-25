:- module(event_getAuthorFeed, [
    onGetAuthorFeed/5,
    insert_record_args/9
]).

/**
Domain event handler for `app.bsky.feed.getAuthorFeed`.

onGetAuthorFeed/5 takes a `pg_session(In, Out)` compound as the
first argument. Connection threading is uniform: the compound's
input slot is the connection the predicate starts with, and after
the body the output slot is bound to either the same connection
(happy path) or a freshly opened one (wire-recovery path).

The per-post catch routes around `pg_query_silently_failed/2` and
`malformed_post/1` by recording the offending post and binding the
session's output slot to the input (no DB state change). Every
other exception is re-raised with the session left in a consistent
state by the underlying client.pl handlers.
*/

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(dif)).
:- use_module(library(serialization/json)).
:- use_module(library(time)).

:- use_module('../../../../../infrastructure/repository/repository_status', [
    exists_by_uri_t/4,
    insert/4
]).
:- use_module('../../../../../infrastructure/repository/repository_popularity', [
    insert_without_unicity_check/3
]).
:- use_module('../../../../../infrastructure/repository/repository_publication', [
    insert/3
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

%% onGetAuthorFeed(+Session, +Cursor, +TotalPosts, +Post, +Index)
%
% Session = pg_session(In, Out). After the call, Out is bound:
% to a fresh connection if a wire reconnect happened during the
% DB calls, or to In otherwise.
onGetAuthorFeed(pg_session(In, Out), Cursor, TotalPosts, Post, Index) :-
    writeln([processing_post_at_index|[(Index/TotalPosts)]], true),
    catch(
        process_post(pg_session(In, Out), Cursor, Post),
        Caught,
        ( ensure_session_out_bound(pg_session(In, Out)),
          on_post_exception(Caught, Index, TotalPosts, Post) )
    ).

%% ensure_session_out_bound(+Session).
%
% Pre-condition for the post-catch handler. On the success path
% process_post binds Out either to In (no reconnect) or to a fresh
% connection (reconnect). On a throw mid-pipeline Out may still be
% unbound -- the two clauses below cover both cases without using
% cut or `->`: var/1 gates clause selection structurally.
ensure_session_out_bound(pg_session(In, Out)) :-
    var(Out),
    Out = In.
ensure_session_out_bound(pg_session(_In, Out)) :-
    nonvar(Out).

%% on_post_exception(+Exception, +Index, +TotalPosts, +Post).
%
% Skips known-safe-to-route-around exceptions, re-raises the rest.
% Pure pattern + dif/2 dispatch; no cuts. Session is bound by
% ensure_session_out_bound/1 before this is called.
on_post_exception(pg_query_silently_failed(SQL, Params), Index, TotalPosts, Post) :-
    record_skipped_post(Index, Post, pg_query_silently_failed(SQL, Params)),
    writeln(
        [skipped_post|[
            index(Index/TotalPosts),
            reason(pg_query_silently_failed)
        ]],
        true
    ).
on_post_exception(malformed_post(Reason), Index, TotalPosts, Post) :-
    record_skipped_post(Index, Post, malformed_post(Reason)),
    writeln(
        [skipped_post|[
            index(Index/TotalPosts),
            reason(malformed_post(Reason))
        ]],
        true
    ).
on_post_exception(Other, _Index, _Total, _Post) :-
    dif(Other, pg_query_silently_failed(_, _)),
    dif(Other, malformed_post(_)),
    throw(Other).

%% process_post(+Session, +Cursor, +Post).
%
% Pipeline: extract fields, dedup by URI, chain status +
% publication + popularity inserts. Session threads through every
% DB call; the outer compound's Out slot is bound at the end of
% the chain (either to the final connection on the success path,
% or by the catch handler in onGetAuthorFeed on the throw path).
process_post(pg_session(In, Out), _Cursor, Post) :-
    insert_record_args(
        Post,
        DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
        likes(LikeCount)-reposts(RepostCount)
    ),
    exists_by_uri_t(pg_session(In, C1), Handle, URI, Exists),
    handle_existence(
        Exists,
        pg_session(C1, Out),
        URI,
        args(DisplayName, Handle, Text, AuthorAvatar, Payload, CreatedAt,
             likes(LikeCount)-reposts(RepostCount))
    ).

%% handle_existence(+Exists, +Session, +URI, +Args).
%
% Pure two-clause dispatch on the reified existence Truth.
handle_existence(true, pg_session(C, C), URI, _Args) :-
    throw(already_indexed_post(uri(URI))).
handle_existence(false, Session, URI,
                 args(DisplayName, Handle, Text, AuthorAvatar, Payload, CreatedAt,
                      likes(LikeCount)-reposts(RepostCount))) :-
    writeln([no_records_found_by_uri|[URI]], true),
    try_inserting_publication_record(
        Session,
        DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
        likes(LikeCount)-reposts(RepostCount),
        _PublicationInsertionResult
    ).

%% insert_record_args(+Post, -DisplayName, -Handle, -Text, -AuthorAvatar, -Payload, -URI, -CreatedAt, -Popularity)
%
% Field extraction goes through assoc_required/3 for fields whose
% absence makes the post unstorable and assoc_optional/4 for fields
% the Bluesky API legitimately omits at zero/empty values.
insert_record_args(
    Post,
    DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
    likes(LikeCount)-reposts(RepostCount)
) :-
    wrapped_pairs_to_assoc(Post, PostAssoc),
    writeln(post:Post),
    writeln(assoc:PostAssoc),

    assoc_required(post, PostAssoc, FeedPost),
    writeln(feed_post:FeedPost),

    assoc_required(author, FeedPost, Author),
    assoc_optional(displayName, Author, DisplayName, []),
    assoc_required(handle, Author, Handle),
    assoc_optional(avatar, Author, AuthorAvatar, []),

    assoc_optional(likeCount, FeedPost, LikeCount, 0),
    assoc_optional(repostCount, FeedPost, RepostCount, 0),

    assoc_required(record, FeedPost, Record),
    assoc_required(text, Record, Text),
    assoc_required(createdAt, Record, CreatedAt),

    assoc_required(uri, FeedPost, URI),
    Payload = FeedPost.

%% assoc_lookup_t(+Key, +Assoc, -Found).
assoc_lookup_t(Key, Assoc, Found) :-
    assoc_to_list(Assoc, KVs),
    pairs_lookup_t(Key, KVs, Found).

pairs_lookup_t(_Key, [], absent).
pairs_lookup_t(Key, [K-V|KVs], Found) :-
    if_(
        Key = K,
        Found = present(V),
        pairs_lookup_t(Key, KVs, Found)
    ).

assoc_required(Key, Assoc, Value) :-
    assoc_lookup_t(Key, Assoc, Found),
    assoc_required_resolve(Key, Found, Value).

assoc_required_resolve(_Key, present(Value), Value).
assoc_required_resolve(Key, absent, _) :-
    throw(malformed_post(missing_field(Key))).

assoc_optional(Key, Assoc, Value, Default) :-
    assoc_lookup_t(Key, Assoc, Found),
    assoc_optional_resolve(Found, Default, Value).

assoc_optional_resolve(present(Value), _Default, Value).
assoc_optional_resolve(absent, Default, Default).

%% try_inserting_publication_record(+Session, ..., -InsertionResult).
%
% Chains status -> publication inserts, threading session through.
% Idempotence is at the DB layer via UNIQUE (hash).
try_inserting_publication_record(
    pg_session(In, Out),
    DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
    likes(LikeCount)-reposts(RepostCount),
    InsertionResult
) :-
    try_inserting_status_record(
        pg_session(In, C1),
        DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
        likes(LikeCount)-reposts(RepostCount),
        RecordId
    ),
    repository_publication:insert(
        pg_session(C1, Out),
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

%% try_inserting_status_record(+Session, ..., -RecordId).
%
% Status insert + popularity insert chained through the session.
try_inserting_status_record(
    pg_session(In, Out),
    DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
    likes(LikeCount)-reposts(RepostCount),
    RecordId
) :-
    writeln([likes_reposts|[likes(LikeCount)-reposts(RepostCount)]], true),
    repository_status:insert(
        pg_session(In, C1),
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
        pg_session(C1, Out),
        row(RecordId, URI, LikeCount, RepostCount),
        _PopularityInsertionResult
    ).

%% record_skipped_post(+Index, +Post, +Reason).
%
% Persist the offending post for downstream replay. I/O errors are
% swallowed so this writer cannot mask the underlying failure it
% is documenting.
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
