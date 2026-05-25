:- module(extract_failing_post, [run/0]).

:- use_module(library(assoc)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/serialization', [
    pairs_to_assoc/2,
    wrapped_pairs_to_assoc/2
]).
:- use_module('../../src/domain/events/app/bsky/feed/event_getAuthorFeed', [
    insert_record_args/9
]).

/*
Extract every field repository_status:insert/3 sees for the post
that fired the SIGSEGV. Reads var/tmp/segv-investigation/crash-<...>/
last-feed-pairs.pl (path via FEED_PAIRS_FIXTURE env), walks the
parsed feed to post index N (env POST_INDEX, default 1 -- the
2nd post, which matches the production log
"processing_post_at_index,1/15" just before the segv), and prints
each field as a re-readable Prolog literal.

Output goes to stdout in a paste-friendly form:

    handle_value("lemonde.fr").
    uri_value("at://did:plc:.../app.bsky.feed.post/...").
    ... etc

so the minimal sigsegv_minimal_repro.pl can `:- include` it or copy
the values inline.
*/

env_chars(Name, Value) :- getenv(Name, Value).

env_chars_default(Name, Default, Value) :-
    (   getenv(Name, V) -> Value = V ; Value = Default ).

load_feed_posts(Posts) :-
    env_chars("FEED_PAIRS_FIXTURE", Path),
    open(Path, read, Stream),
    catch(
        read_term(Stream, Term, []),
        E,
        (close(Stream), throw(E))
    ),
    close(Stream),
    Term = last_feed_pairs(ResponsePairs),
    pairs_to_assoc(ResponsePairs, FeedAssoc),
    get_assoc(feed, FeedAssoc, Posts).

post_at_index(Posts, IndexChars, Post) :-
    number_chars(I, IndexChars),
    nth0(I, Posts, Post).

emit_chars_value(Name, Chars) :-
    format("~w(~q).~n", [Name, Chars]).

run :-
    env_chars_default("POST_INDEX", "1", IndexChars),
    load_feed_posts(Posts),
    length(Posts, Total),
    format("% total_posts(~w).~n", [Total]),
    post_at_index(Posts, IndexChars, Post),
    insert_record_args(
        Post,
        DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt,
        likes(LikeCount)-reposts(RepostCount)
    ),
    format("% index(~s).~n", [IndexChars]),
    emit_chars_value(display_name_value, DisplayName),
    emit_chars_value(handle_value, Handle),
    emit_chars_value(text_value, Text),
    emit_chars_value(author_avatar_value, AuthorAvatar),
    emit_chars_value(payload_value, Payload),
    emit_chars_value(uri_value, URI),
    emit_chars_value(created_at_value, CreatedAt),
    format("like_count_value(~w).~n", [LikeCount]),
    format("repost_count_value(~w).~n", [RepostCount]).

:- initialization(run).
