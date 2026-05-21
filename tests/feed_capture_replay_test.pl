:- module(feed_capture_replay_test, [run_test/0]).

:- use_module(library(assoc)).
:- use_module(library(format)).
:- use_module(library(lists)).

:- use_module('../src/serialization', [
    pairs_to_assoc/2
]).
:- use_module('../src/domain/events/app/bsky/feed/event_getAuthorFeed', [
    insert_record_args/9
]).

/*
Hermetic replay of a production getAuthorFeed response over the
JSON-decoding path that feeds onGetAuthorFeed/2.

Fixture
-------

tests/fixtures/segv-investigation/last-feed-pairs.pl is a verbatim
copy of /tmp/segv-investigation/last-feed-pairs.pl pulled from
io.marianne.caprica at the moment the worker crashed in
matching_criteria/4 (lefigaro.fr @ 2026-05-18T17:30:00Z). It holds
the parsed JSON-DCG pair tree of the in-flight feed response, so
the test exercises the exact data shape that was in scryer when
the SIGSEGV fired.

What this guards against
------------------------

  * `insert_record_args/9` silently failing on a real production
    post shape -- one of the candidate root causes of the
    `onGetAuthorFeed_failed_with_posts_count|N` log entry, since
    a maplist whose goal fails silently is precisely what trips
    the fallback clause of iterate_or_report_failure/5.

This test is intentionally DB-free and HTTP-free: it only walks
the captured pair tree and runs the decoder. The SIGSEGV itself
is a scryer-internal issue tracked separately (see
tests/pg/segv_replay_test.pl); this regression covers the Prolog
side independently.
*/

capture_path("./tests/fixtures/segv-investigation/last-feed-pairs.pl").

load_response_pairs(ResponsePairs) :-
    capture_path(Path),
    open(Path, read, Stream),
    catch(
        read_term(Stream, Term, []),
        E,
        (close(Stream), throw(E))
    ),
    close(Stream),
    Term = last_feed_pairs(ResponsePairs).

load_feed_posts(Posts) :-
    load_response_pairs(ResponsePairs),
    pairs_to_assoc(ResponsePairs, FeedAssoc),
    get_assoc(feed, FeedAssoc, Posts).

decode_post(Post, Outcome) :-
    catch(
        (   insert_record_args(
                Post,
                _DisplayName, _Handle, _Text, _AuthorAvatar, _Payload, _URI, _CreatedAt,
                likes(_LikeCount)-reposts(_RepostCount)
            )
        ->  Outcome = ok
        ;   Outcome = ko_silent_fail
        ),
        E,
        Outcome = ko_thrown(E)
    ).

% test_feed_has_posts/0
%
% Sanity check on the fixture itself: load_feed_posts/1 must yield
% a non-empty list. If this fires [KO] the fixture is wrong, not
% the production code under test.
test_feed_has_posts :-
    load_feed_posts(Posts),
    length(Posts, N),
    (   N >= 1
    ->  format("[OK] feed_capture_replay:captured_feed_has_posts (~w posts)~n", [N])
    ;   format("[KO] feed_capture_replay:captured_feed_has_posts -- fixture has 0 posts~n", []),
        halt(1)
    ).

% test_every_post_decodes_cleanly/0
%
% Every captured post must run through insert_record_args/9 without
% silently failing or throwing. A silent fail here is the exact
% symptom reproduced from the production capture: maplist's per-post
% goal returning false without an exception, which trips
% iterate_or_report_failure/5's fallback clause upstream.
test_every_post_decodes_cleanly :-
    load_feed_posts(Posts),
    maplist(decode_post, Posts, Outcomes),
    classify(Outcomes, 0-0-0, OkCount-FailCount-ThrowCount),
    length(Posts, Total),
    (   FailCount =:= 0,
        ThrowCount =:= 0
    ->  format("[OK] feed_capture_replay:every_post_decodes_cleanly (ok=~w/~w)~n",
               [OkCount, Total])
    ;   format("[KO] feed_capture_replay:every_post_decodes_cleanly ok=~w fail=~w throw=~w (~w posts)~n",
               [OkCount, FailCount, ThrowCount, Total]),
        report_failures(Outcomes, 1),
        halt(1)
    ).

classify([], Acc, Acc).
classify([ok|Rest], Ok-F-T, Final) :-
    Ok1 is Ok + 1,
    classify(Rest, Ok1-F-T, Final).
classify([ko_silent_fail|Rest], Ok-F-T, Final) :-
    F1 is F + 1,
    classify(Rest, Ok-F1-T, Final).
classify([ko_thrown(_)|Rest], Ok-F-T, Final) :-
    T1 is T + 1,
    classify(Rest, Ok-F-T1, Final).

report_failures([], _).
report_failures([ok|Rest], I) :-
    I1 is I + 1, report_failures(Rest, I1).
report_failures([ko_silent_fail|Rest], I) :-
    format("     post[~w]: silent fail~n", [I]),
    I1 is I + 1, report_failures(Rest, I1).
report_failures([ko_thrown(E)|Rest], I) :-
    format("     post[~w]: thrown ~q~n", [I, E]),
    I1 is I + 1, report_failures(Rest, I1).

run_test :-
    test_feed_has_posts,
    test_every_post_decodes_cleanly.

:- initialization(run_test).
