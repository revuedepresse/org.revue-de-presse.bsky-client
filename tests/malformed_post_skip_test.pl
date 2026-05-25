:- module(malformed_post_skip_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).

:- use_module('../src/domain/events/app/bsky/feed/event_getAuthorFeed', [
    insert_record_args/9
]).

/*
Regression: insert_record_args/9 must not silently fail when the
Bluesky API omits a field. The bare get_assoc/3 chain it used to
run aborted the maplist over the page feed on the first post that
came back without likeCount / repostCount / displayName / avatar,
and the outer iterate_or_report_failure/6 then surfaced the page
as maplist_silently_failed_over_feed(15) at the worker boundary.

The captured production trace for lemonde.fr (PG_BACKEND=wire,
2026-05-25) showed two posts already-indexed throws followed by
maplist_silently_failed_over_feed(15), consistent with a feed
where one mid-page post triggered the silent assoc miss and the
deeper-page failure unwind then backtracked through the catch
handler's reif:=/3 second solution.

Cases:

  1. Post missing likeCount / repostCount / displayName / avatar
     -- the four fields the Bluesky API omits at zero/empty
     values. insert_record_args/9 must succeed with defaults
     (0 / 0 / [] / []) instead of silently failing.

  2. Post missing record.text -- a record without text is a
     repost / quote shape we cannot store. insert_record_args/9
     must throw malformed_post(missing_field(text)) so the
     per-post catch in onGetAuthorFeed/4 routes around it.

  3. Post missing top-level uri -- structural breakage. Must
     throw malformed_post(missing_field(uri)).
*/

%% Construct a synthetic feed post with full author + record.
make_complete_post(Post) :-
    Post = pairs([
        string("post")-pairs([
            string("uri")-string("at://probe-complete"),
            string("author")-pairs([
                string("did")-string("did:plc:probe"),
                string("handle")-string("probe.bsky.social"),
                string("displayName")-string("Probe"),
                string("avatar")-string("https://probe.example/x.png")
            ]),
            string("likeCount")-number(5),
            string("repostCount")-number(2),
            string("record")-pairs([
                string("text")-string("hello"),
                string("createdAt")-string("2026-05-25T00:00:00Z")
            ])
        ])
    ]).

%% Same shape, but the four Bluesky-omitted-at-zero fields are
%% absent from both the author and the post object.
make_post_missing_optional_fields(Post) :-
    Post = pairs([
        string("post")-pairs([
            string("uri")-string("at://probe-optional"),
            string("author")-pairs([
                string("did")-string("did:plc:probe"),
                string("handle")-string("probe.bsky.social")
            ]),
            string("record")-pairs([
                string("text")-string("hello"),
                string("createdAt")-string("2026-05-25T00:00:00Z")
            ])
        ])
    ]).

%% record.text deliberately absent (mimics a repost/quote record).
make_post_missing_record_text(Post) :-
    Post = pairs([
        string("post")-pairs([
            string("uri")-string("at://probe-no-text"),
            string("author")-pairs([
                string("did")-string("did:plc:probe"),
                string("handle")-string("probe.bsky.social"),
                string("displayName")-string("Probe"),
                string("avatar")-string("https://probe.example/x.png")
            ]),
            string("likeCount")-number(0),
            string("repostCount")-number(0),
            string("record")-pairs([
                string("createdAt")-string("2026-05-25T00:00:00Z")
            ])
        ])
    ]).

%% Top-level uri absent.
make_post_missing_uri(Post) :-
    Post = pairs([
        string("post")-pairs([
            string("author")-pairs([
                string("did")-string("did:plc:probe"),
                string("handle")-string("probe.bsky.social"),
                string("displayName")-string("Probe"),
                string("avatar")-string("https://probe.example/x.png")
            ]),
            string("likeCount")-number(0),
            string("repostCount")-number(0),
            string("record")-pairs([
                string("text")-string("hello"),
                string("createdAt")-string("2026-05-25T00:00:00Z")
            ])
        ])
    ]).

%% --- Case 1: optional fields default cleanly ----------------------
case_optional_defaults(Result) :-
    make_post_missing_optional_fields(Post),
    catch(
        insert_record_args(
            Post,
            DisplayName, Handle, Text, AuthorAvatar, _Payload, URI, CreatedAt,
            likes(LikeCount)-reposts(RepostCount)
        ),
        Err,
        Caught = thrown(Err)
    ),
    (   var(Caught),
        DisplayName == [],
        AuthorAvatar == [],
        LikeCount == 0,
        RepostCount == 0,
        Handle == "probe.bsky.social",
        URI == "at://probe-optional",
        Text == "hello",
        CreatedAt == "2026-05-25T00:00:00Z"
    ->  Result = ok,
        format("[ok] case1: optional fields defaulted (display=[], avatar=[], likes=0, reposts=0)~n", [])
    ;   nonvar(Caught)
    ->  Result = ko(case1_threw(Caught)),
        format("[KO] case1: insert_record_args/9 threw ~q (should have defaulted)~n", [Caught])
    ;   Result = ko(case1_wrong_bindings(
                        display(DisplayName), avatar(AuthorAvatar),
                        likes(LikeCount), reposts(RepostCount))),
        format("[KO] case1: defaults wrong: display=~q avatar=~q likes=~q reposts=~q~n",
               [DisplayName, AuthorAvatar, LikeCount, RepostCount])
    ).

%% --- Case 2: missing record.text -> labelled throw ----------------
case_missing_text_throws(Result) :-
    make_post_missing_record_text(Post),
    catch(
        insert_record_args(
            Post,
            _DisplayName, _Handle, _Text, _Avatar, _Payload, _URI, _CreatedAt,
            likes(_LikeCount)-reposts(_RepostCount)
        ),
        Err,
        Caught = thrown(Err)
    ),
    (   Caught == thrown(malformed_post(missing_field(text)))
    ->  Result = ok,
        format("[ok] case2: missing record.text routed as malformed_post(missing_field(text))~n", [])
    ;   var(Caught)
    ->  Result = ko(case2_no_throw),
        format("[KO] case2: insert_record_args/9 silently succeeded on missing text -- bug still present~n", [])
    ;   Result = ko(case2_wrong_throw(Caught)),
        format("[KO] case2: wrong throw: ~q~n", [Caught])
    ).

%% --- Case 3: missing post.uri -> labelled throw -------------------
case_missing_uri_throws(Result) :-
    make_post_missing_uri(Post),
    catch(
        insert_record_args(
            Post,
            _DisplayName, _Handle, _Text, _Avatar, _Payload, _URI, _CreatedAt,
            likes(_LikeCount)-reposts(_RepostCount)
        ),
        Err,
        Caught = thrown(Err)
    ),
    (   Caught == thrown(malformed_post(missing_field(uri)))
    ->  Result = ok,
        format("[ok] case3: missing post.uri routed as malformed_post(missing_field(uri))~n", [])
    ;   var(Caught)
    ->  Result = ko(case3_no_throw),
        format("[KO] case3: insert_record_args/9 silently succeeded on missing uri~n", [])
    ;   Result = ko(case3_wrong_throw(Caught)),
        format("[KO] case3: wrong throw: ~q~n", [Caught])
    ).

%% --- Case 4: happy path still works -------------------------------
case_complete_post_works(Result) :-
    make_complete_post(Post),
    catch(
        insert_record_args(
            Post,
            DisplayName, Handle, Text, AuthorAvatar, _Payload, URI, CreatedAt,
            likes(LikeCount)-reposts(RepostCount)
        ),
        Err,
        Caught = thrown(Err)
    ),
    (   var(Caught),
        DisplayName == "Probe",
        Handle == "probe.bsky.social",
        AuthorAvatar == "https://probe.example/x.png",
        LikeCount == 5,
        RepostCount == 2,
        URI == "at://probe-complete",
        Text == "hello",
        CreatedAt == "2026-05-25T00:00:00Z"
    ->  Result = ok,
        format("[ok] case4: complete post still extracts every field~n", [])
    ;   nonvar(Caught)
    ->  Result = ko(case4_threw(Caught)),
        format("[KO] case4: complete post threw ~q~n", [Caught])
    ;   Result = ko(case4_wrong_bindings),
        format("[KO] case4: bindings wrong: display=~q handle=~q likes=~q reposts=~q~n",
               [DisplayName, Handle, LikeCount, RepostCount])
    ).

run_test :-
    case_optional_defaults(R1),
    case_missing_text_throws(R2),
    case_missing_uri_throws(R3),
    case_complete_post_works(R4),
    (   R1 == ok, R2 == ok, R3 == ok, R4 == ok
    ->  format("[OK] all 4 malformed-post-skip cases passed~n", []),
        halt(0)
    ;   format("[KO] one or more cases failed: r1=~q r2=~q r3=~q r4=~q~n", [R1, R2, R3, R4]),
        halt(1)
    ).

:- initialization(run_test).
