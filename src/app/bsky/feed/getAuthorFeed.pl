:- module('getAuthorFeed', [
    app__bsky__feed__getAuthorFeed/2,
    report_iteration_failure/1
]).

:- use_module(library(assoc)).
:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(http/http_open)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(reif)).
:- use_module(library(si)).
:- use_module(library(serialization/json)).
:- use_module(library(time)).

:- use_module('../../../domain/events/app/bsky/feed/event_getAuthorFeed', [
    onGetAuthorFeed/5
]).
:- use_module('../../../infrastructure/pg/connection', [
    open_pg_connection/1,
    close_pg_connection/1
]).
:- use_module('../../../configuration', [
    credentials_access_jwt/1
]).
:- use_module('../../../http', [
    public_bluesky_appview_api_endpoint/2,
    header_content_type_application_json/1
]).
:- use_module('../../../logger', [
    log_debug/1,
    log_error/1,
    log_info/1
]).
:- use_module('../../../parameters', [
    ensures_query_parameter/1,
    extract_single_required_parameter/2
]).
:- use_module('../../../serialization', [
    by_key/3,
    keys/3,
    pairs_to_assoc/2,
    to_json_chars/2,
    wrapped_pairs_to_assoc/2
]).
:- use_module('../../../stream', [
    read_stream/2,
    writeln/1,
    writeln/2
]).
:- use_module('../../../string', [concat_as_string/3]).
:- use_module('../../../api', [endpoint_spec_pairs/2]).
:- use_module('../../../temporal', [
    date_iso8601_days_ago/2,
    date_iso8601_days_before/3
]).

%% app__bsky__feed__getAuthorFeed_endpoint(+OperationId, +ParamName, +Param, -Endpoint).
app__bsky__feed__getAuthorFeed_endpoint(OperationId, ParamName, Param, Endpoint) :-
    public_bluesky_appview_api_endpoint(OperationId, EndpointWithoutParam),
    concat_as_string([EndpointWithoutParam, "?limit=15&filter=posts_no_replies&includePins=false&", ParamName, "=", Param], [], Endpoint).

%% app__bsky__feed__getAuthorFeed_headers(-ListHeaders).
app__bsky__feed__getAuthorFeed_headers(ListHeaders) :-
    header_content_type_application_json(ApplicationJsonContentTypeHeader),

    credentials_access_jwt(AccessJwt),
    append(["Bearer ", AccessJwt], BearerTokenChars),
    atom_chars(BearerToken, BearerTokenChars),

    ListHeaders = [
        ApplicationJsonContentTypeHeader,
        'Authorization'(BearerToken),
        'User-Agent'('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36')
    ].

%% send_request(+Session, +Params, +AnchorIn, -AnchorOut, -ResponsePairs, -StatusCode).
send_request(Session, Params, AnchorIn, AnchorOut, ResponsePairs, StatusCode) :-
    endpoint_spec_pairs(SpecPairs, false),

    pairs_keys(SpecPairs, [string(Verb)]),
    pairs_to_assoc(SpecPairs, SpecAssoc),

    chars_si(Verb),
    atom_chars(VerbAtom, Verb),
    get_assoc(VerbAtom, SpecAssoc, EndpointSpecAssoc),
    get_assoc(parameters, EndpointSpecAssoc, Parameters),
    get_assoc(operationId, EndpointSpecAssoc, OperationId),

    extract_single_required_parameter(Parameters, RequiredParameter),
    get_assoc(name, RequiredParameter, ParamName),
    ensures_query_parameter(RequiredParameter),

    app__bsky__feed__getAuthorFeed_headers(ListHeaders),

    Options = [
        method(VerbAtom),
        status_code(StatusCode),
        request_headers(ListHeaders),
        headers(ResponseHeaders)
    ],

    once(resolve_endpoint(Params, OperationId, ParamName, ParamValue, Endpoint)),

    submit_request_once(
        request(Endpoint, Options),
        response(ResponseHeaders, Stream)
    ),
    read_stream(Stream, BodyChars),
    writeln([status_code|[StatusCode]], true),

    current_time(T), phrase(format_time("%Y/%m/%d (%H:%M:%S)", T), Time),
    writeln([current_time|[Time]], true),

    phrase(json_chars(pairs(ResponsePairs)), BodyChars),
    catch(capture_feed_response(BodyChars, ResponsePairs), _, true),
    pairs_to_assoc(ResponsePairs, FeedAssoc),
    get_assoc(feed, FeedAssoc, Feed),

    once(set_anchor_if_unset(AnchorIn, Feed, AnchorOut)),
    once(resolve_next_cursor(FeedAssoc, NextCursor)),
    writeln([next_cursor|[NextCursor]], true),

    length(Feed, HowManyPostsInFeed),
    numlist(HowManyPostsInFeed, IndicesStartingAt1),

    once(iterate_or_report_failure(Session, NextCursor, HowManyPostsInFeed, Feed, IndicesStartingAt1, ParamValue, AnchorOut)),

    append([OperationId, " call failed"], FailedHttpRequestErrorMessage),
    chars_si(FailedHttpRequestErrorMessage),
    atom_chars(FailedHttpRequestErrorMessageAtom, FailedHttpRequestErrorMessage),

    if_(
        StatusCode = 200,
        true,
        throw(failed_http_request(FailedHttpRequestErrorMessageAtom, ResponsePairs, StatusCode))
    ).

    %% submit_request_once(+request, -Response).
    submit_request_once(request(Endpoint, Options), response(ResponseHeaders, Stream)) :-
        writeln([endpoint|[Endpoint]], true),
        once(submit_open_or_throw(Endpoint, Stream, Options, ResponseHeaders)).

    submit_open_or_throw(Endpoint, Stream, Options, ResponseHeaders) :-
        http_open(Endpoint, Stream, Options),
        writeln(response_headers: ResponseHeaders).
    submit_open_or_throw(Endpoint, _Stream, Options, _ResponseHeaders) :-
        \+ http_open(Endpoint, _, Options),
        throw(failed_http_request(Endpoint, Options)).

resolve_endpoint(actor(ParamValue)-cursor(Cursor), OperationId, ParamName, ParamValue, Endpoint) :-
    app__bsky__feed__getAuthorFeed_endpoint(OperationId, ParamName, ParamValue, EndpointWithoutCursor),
    append([EndpointWithoutCursor, "&cursor=", Cursor], Endpoint).
resolve_endpoint(Params, OperationId, ParamName, Params, Endpoint) :-
    \+ ( Params = actor(_)-cursor(_) ),
    app__bsky__feed__getAuthorFeed_endpoint(OperationId, ParamName, Params, Endpoint).

resolve_next_cursor(FeedAssoc, NextCursor) :- get_assoc(cursor, FeedAssoc, NextCursor).
resolve_next_cursor(FeedAssoc, 'none') :- \+ get_assoc(cursor, FeedAssoc, _).

% One-shot dump of the latest HTTP body and the parsed pair list
% to disk, used to assemble a reproducer for the scryer
% unify_constant SIGSEGV. Overwrites the same files every call.
capture_feed_response(BodyChars, ResponsePairs) :-
    BodyPath  = "/tmp/segv-investigation/last-feed-body.txt",
    PairsPath = "/tmp/segv-investigation/last-feed-pairs.pl",
    open(BodyPath, write, BodyStream, [type(text)]),
    write(BodyStream, BodyChars),
    close(BodyStream),
    open(PairsPath, write, PairsStream, [type(text)]),
    write_canonical(PairsStream, last_feed_pairs(ResponsePairs)),
    write(PairsStream, '.'),
    nl(PairsStream),
    close(PairsStream).

%% iterate_or_report_failure(+Session, +NextCursor, +Total, +Feed, +Indices, +ParamValue, +Anchor)
%
% Session = pg_session(In, Out). Folds the feed through
% onGetAuthorFeed/5 threading the connection from one post to the
% next, then recurses into follow_or_finalize/4 (which may
% paginate to the next page through the same session).
%
% Already-indexed posts stop the per-page fold but not the
% pagination: the cursor-based recursion continues from the next
% page. This preserves the pre-refactor behaviour of bailing out
% of the page after the first already-known post while keeping the
% per-post catch in onGetAuthorFeed/5 active for everything else
% (pg_query_silently_failed and malformed_post are routed to
% skipped_post entries, unknown exceptions propagate).
iterate_or_report_failure(pg_session(In, OutFinal), NextCursor, Total, Feed, Indices, ParamValue, Anchor) :-
    fold_feed(pg_session(In, OutFold), Total, NextCursor, Feed, Indices),
    follow_or_finalize(pg_session(OutFold, OutFinal), NextCursor, ParamValue, Anchor).

%% fold_feed(+Session, +Total, +NextCursor, +Feed, +Indices)
%
% Session = pg_session(In, Out). Threads the connection through
% one onGetAuthorFeed/5 call per post. The catch handler dispatches
% on whether the call's session output (C1) was already bound by
% the inner pipeline (nonvar -> use as-is) or not (var -> set to
% the input). Pure two-clause dispatch via `already_indexed_handler/4`;
% no cuts.
fold_feed(pg_session(C, C), _Total, _NextCursor, [], _Indices).
fold_feed(pg_session(C0, COut), Total, NextCursor, [Post|Rest], [Idx|IdxRest]) :-
    catch(
        ( onGetAuthorFeed(pg_session(C0, C1), NextCursor, Total, Post, Idx),
          fold_feed(pg_session(C1, COut), Total, NextCursor, Rest, IdxRest)
        ),
        already_indexed_post(URI),
        already_indexed_handler(URI, C0, C1, COut)
    ).

already_indexed_handler(URI, _C0, C1, C1) :-
    nonvar(C1),
    writeln([post_indexed_before|[URI]], true).
already_indexed_handler(URI, C0, C1, C0) :-
    var(C1),
    C1 = C0,
    writeln([post_indexed_before|[URI]], true).

% Fallback for the maplist-over-feed path: log the count then
% propagate a labelled exception so the caller's catch in
% app__bsky__feed__getAuthorFeed/2 surfaces the silent maplist
% failure as a real error instead of letting the worker move on
% as if the page processed cleanly.
report_iteration_failure(HowManyPostsInFeed) :-
    writeln([onGetAuthorFeed_failed_with_posts_count|HowManyPostsInFeed], true),
    throw(maplist_silently_failed_over_feed(HowManyPostsInFeed)).

% Cutoff anchor for getAuthorFeed pagination. Threaded through the
% recursion as a value (the ISO-8601 most-recent indexedAt on the
% first non-empty page) or `none` until one is seen. No process
% global state.

% set_anchor_if_unset(+AnchorIn, +Feed, -AnchorOut).
set_anchor_if_unset(AnchorIso, _Feed, AnchorIso) :-
    dif(AnchorIso, none).
set_anchor_if_unset(none, [FirstWrapped|_], FirstIndexedAt) :-
    wrapped_pairs_to_assoc(FirstWrapped, FirstAssoc),
    get_assoc(post, FirstAssoc, FirstPost),
    get_assoc(indexedAt, FirstPost, FirstIndexedAt),
    writeln([cutoff_anchor_set|[FirstIndexedAt]], true).
set_anchor_if_unset(none, [], none) :-
    writeln([cutoff_anchor_skipped_empty_feed], true).

follow_or_finalize(pg_session(C, C), 'none', _ParamValue, _Anchor) :-
    writeln('fetched all available feed posts', true).
follow_or_finalize(pg_session(In, Out), NextCursor, ParamValue, Anchor) :-
    dif(NextCursor, 'none'),
    once(resolve_cutoff_date(Anchor, CutoffDate)),
    cursor_date_prefix(NextCursor, CursorDate),
    once(continue_or_stop_at_cutoff(pg_session(In, Out), CursorDate, CutoffDate, NextCursor, ParamValue, Anchor)).

    % resolve_cutoff_date(+Anchor, -CutoffDate).
    %
    % Cutoff = (anchor - 2 days) when an anchor was captured on a
    % prior page (anchor = first page's most-recent indexedAt).
    % Falls back to (now - 2 days) when no anchor is available.
    resolve_cutoff_date(none, CutoffDate) :-
        date_iso8601_days_ago(2, CutoffIso),
        cursor_date_prefix(CutoffIso, CutoffDate).
    resolve_cutoff_date(AnchorIso, CutoffDate) :-
        dif(AnchorIso, none),
        date_iso8601_days_before(2, AnchorIso, CutoffDate).

    % cursor_date_prefix(+IsoChars, -DatePrefix).
    %
    % Takes the leading "YYYY-MM-DD" of an ISO-8601 timestamp.
    % Day precision is enough for the 2-day cutoff and sidesteps the
    % fractional-seconds lex-compare hazard ("...:00.5Z" vs "...:00Z").
    cursor_date_prefix(IsoChars, DatePrefix) :-
        length(DatePrefix, 10),
        append(DatePrefix, _, IsoChars).

    continue_or_stop_at_cutoff(pg_session(C, C), CursorDate, CutoffDate, NextCursor, _ParamValue, _Anchor) :-
        CursorDate @< CutoffDate,
        writeln([stopped_cursor_more_than_two_days_before_anchor|[NextCursor, cutoff(CutoffDate)]], true).
    continue_or_stop_at_cutoff(pg_session(In, Out), CursorDate, CutoffDate, NextCursor, ParamValue, Anchor) :-
        \+ (CursorDate @< CutoffDate),
        NextParams = actor(ParamValue)-cursor(NextCursor),
        app__bsky__feed__getAuthorFeed_paginate(pg_session(In, Out), NextParams, Anchor, _AnchorOut, _Props).

%% app__bsky__feed__getAuthorFeed(+Params, -Props).
%
% [app.bsky.feed.getAuthorFeed](https://docs.bsky.app/docs/api/app-bsky-feed-get-author-feed)
%
% Pagination walks distinct cursors, so the per-(actor,cursor) cache
% would never hit on a normal traversal; the kept-around dynamic
% memo table also exceeded scryer's 255 max_arity on every real
% response and crashed the worker. The predicate now delegates
% straight to the network call.
app__bsky__feed__getAuthorFeed(Params, Props) :-
    open_pg_connection(Conn0),
    catch(
        ( app__bsky__feed__getAuthorFeed_paginate(pg_session(Conn0, ConnFinal), Params, none, _AnchorOut, Props),
          close_pg_connection(ConnFinal) ),
        Err,
        ( best_effort_close_session(Conn0, ConnFinal),
          throw(Err) )
    ).

%% best_effort_close_session(+Conn0, +ConnFinal)
%
% On a throw mid-pagination we may have an unbound ConnFinal (the
% pipeline never reached the bind point). Close whatever we have:
% Conn0 always, ConnFinal too if a fresh one was opened. The two
% clauses dispatch on whether ConnFinal is bound; no cuts.
best_effort_close_session(Conn0, ConnFinal) :-
    nonvar(ConnFinal),
    close_pg_connection(ConnFinal),
    close_pg_connection_if_distinct(Conn0, ConnFinal).
best_effort_close_session(Conn0, ConnFinal) :-
    var(ConnFinal),
    close_pg_connection(Conn0).

close_pg_connection_if_distinct(Conn0, Conn0).
close_pg_connection_if_distinct(Conn0, ConnFinal) :-
    dif(Conn0, ConnFinal),
    close_pg_connection(Conn0).

%% app__bsky__feed__getAuthorFeed_paginate(+Session, +Params, +AnchorIn, -AnchorOut, -Props).
app__bsky__feed__getAuthorFeed_paginate(pg_session(In, Out), Params, AnchorIn, AnchorOut, Props) :-
    catch(
        send_request(pg_session(In, Out), Params, AnchorIn, AnchorOut, Pairs, StatusCode),
        E,
        if_(
            E = failed_http_request(Message, Pairs, StatusCode),
            (log_error([Message]), fail),
            (log_error([E]), fail)
        )
    ),

    if_(
        dif(StatusCode, 200),
       (by_key("message", Pairs, ErrorMessageChars),
        chars_si(ErrorMessageChars),
        atom_chars(ErrorMessage, ErrorMessageChars),
        log_error([ErrorMessage]), fail),
        Props = Pairs
    ).
