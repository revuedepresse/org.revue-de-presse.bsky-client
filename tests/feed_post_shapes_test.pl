:- module(feed_post_shapes_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).

:- use_module('../src/domain/events/app/bsky/feed/event_getAuthorFeed', [
    insert_record_args/9
]).

/*
Regression: two feed post shapes made getAuthorFeed give up on the
rest of an account's feed.

  1. A JSON null anywhere in the post (`"external": null` in a
     quoted post's embed): insert_record_args/9 must decode the post.

  2. An author handle longer than 32 characters, as handles bridged
     from Mastodon are (<user>.<instance>.ap.brid.gy).
     publication.screen_name and weaving_status.ust_full_name are
     varchar(32): insert_record_args/9 must label such a post
     malformed so the per-post catch skips it; a handle of exactly
     32 characters still fits.
*/

make_post(Handle, Embed, Post) :-
    Post = pairs([
        string("post")-pairs([
            string("uri")-string("at://did:plc:probe/app.bsky.feed.post/probe"),
            string("author")-pairs([
                string("did")-string("did:plc:probe"),
                string("handle")-string(Handle)
            ]),
            string("likeCount")-number(1),
            string("repostCount")-number(1),
            string("record")-pairs([
                string("text")-string("hello"),
                string("createdAt")-string("2026-05-25T00:00:00Z")
            ]),
            string("embed")-Embed
        ])
    ]).

null_embed(pairs([
    string("record")-pairs([
        string("value")-pairs([
            string("embed")-pairs([
                string("external")-null,
                string("record")-null
            ])
        ])
    ])
])).

no_embed(pairs([])).

args(Post, Outcome) :-
    catch(
        ( insert_record_args(Post, _, Handle, _, _, _, _, _, _)
        -> Outcome = decoded(Handle)
        ;  Outcome = failed ),
        Error,
        Outcome = thrown(Error)
    ).

check(Label, Outcome, Expected) :-
    (   Outcome = Expected
    ->  format("[ok] ~s~n", [Label])
    ;   format("[KO] ~s: ~q~n", [Label, Outcome]) ).

run_test :-
    null_embed(NullEmbed),
    no_embed(NoEmbed),
    make_post("probe.bsky.social", NullEmbed, NullPost),
    args(NullPost, NullOutcome),
    check("a post holding JSON nulls decodes", NullOutcome, decoded("probe.bsky.social")),

    NullName = pairs([
        string("post")-pairs([
            string("uri")-string("at://did:plc:probe/app.bsky.feed.post/probe"),
            string("author")-pairs([
                string("handle")-string("probe.bsky.social"),
                string("displayName")-null
            ]),
            string("record")-pairs([
                string("text")-string("hello"),
                string("createdAt")-string("2026-05-25T00:00:00Z")
            ])
        ])
    ]),
    catch(insert_record_args(NullName, DisplayName, _, _, _, _, _, _, _), E, DisplayName = thrown(E)),
    check("a null optional field takes its default", DisplayName, []),

    NullText = pairs([
        string("post")-pairs([
            string("uri")-string("at://did:plc:probe/app.bsky.feed.post/probe"),
            string("author")-pairs([string("handle")-string("probe.bsky.social")]),
            string("record")-pairs([
                string("text")-null,
                string("createdAt")-string("2026-05-25T00:00:00Z")
            ])
        ])
    ]),
    args(NullText, NullTextOutcome),
    check("a null required field labels the post malformed",
          NullTextOutcome, thrown(malformed_post(missing_field(text)))),

    length(Fits, 32), maplist(=(a), Fits),
    make_post(Fits, NoEmbed, FitsPost),
    args(FitsPost, FitsOutcome),
    check("a 32-character handle is kept", FitsOutcome, decoded(Fits)),

    Long = "someone.mastodon.social.ap.brid.gy",
    make_post(Long, NoEmbed, LongPost),
    args(LongPost, LongOutcome),
    check("a handle over 32 characters is labelled malformed",
          LongOutcome, thrown(malformed_post(handle_too_long(Long)))),
    format("[OK] feed_post_shapes_test done~n", []).

:- initialization(run_test).
