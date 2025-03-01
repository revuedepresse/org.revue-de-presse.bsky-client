:- module('getAuthorFeed', [
    app__bsky__feed__getAuthorFeed/2
]).

:- use_module(library(assoc)).
:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(http/http_open)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(reif)).
:- use_module(library(si)).
:- use_module(library(serialization/json)).
:- use_module(library(time)).

:- use_module('../../../domain/events/app/bsky/feed/event_getAuthorFeed', [
    onGetAuthorFeed/1,
    onGetAuthorFeed/2,
    onGetAuthorFeed/4
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
    to_json_chars/2
]).
:- use_module('../../../stream', [
    read_stream/2,
    writeln/1,
    writeln/2
]).
:- use_module('../../../string', [concat_as_string/3]).
:- use_module('../../../api', [endpoint_spec_pairs/2]).

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

%% send_request(+Params, -ResponsePairs, -StatusCode).
send_request(Params, ResponsePairs, StatusCode) :-
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

    (   Params = (actor(ParamValue)-cursor(Cursor))
    ->  app__bsky__feed__getAuthorFeed_endpoint(OperationId, ParamName, ParamValue, EndpointWithoutCursor),
        append([EndpointWithoutCursor, "&cursor=", Cursor], Endpoint)
    ;   ParamValue = Params,
        app__bsky__feed__getAuthorFeed_endpoint(OperationId, ParamName, ParamValue, Endpoint) ),

    submit_request_once(
        request(Endpoint, Options),
        response(ResponseHeaders, Stream)
    ),
    read_stream(Stream, BodyChars),
    writeln(status_code(StatusCode), true),

    phrase(json_chars(pairs(ResponsePairs)), BodyChars),
    pairs_to_assoc(ResponsePairs, FeedAssoc),
    get_assoc(feed, FeedAssoc, Feed),

    (   get_assoc(cursor, FeedAssoc, NextCursor)
    ->  true
    ;   NextCursor = 'none' ),
    writeln([next_cursor|[NextCursor]], true),

    length(Feed, HowManyPostsInFeed),
    numlist(HowManyPostsInFeed, IndicesStartingAt1),

    (   catch(
            maplist(onGetAuthorFeed(NextCursor, HowManyPostsInFeed), Feed, IndicesStartingAt1),
            E,
            if_(
                E = already_indexed_post(URI),
                writeln(post(URI)-was_indexed_before, true),
                throw(could_not_iterate_over_all_author_feed_post(E))
            )
        )
    ->  if_(
            NextCursor = 'none',
            writeln('fetched all available feed posts', true),
           (NextParams = actor(ParamValue)-cursor(NextCursor),
            app__bsky__feed__getAuthorFeed(NextParams, _Props))
        )
    ;   writeln([onGetAuthorFeed_failed_with_posts_count|HowManyPostsInFeed], true) ),

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
        once((
            writeln([endpoint|[Endpoint]], true),
            (   once(http_open(Endpoint, Stream, Options))
            ->  writeln(response_headers: ResponseHeaders)
            ;   throw(failed_http_request(Endpoint, Options)) )
        )).

:- dynamic(app__bsky__feed__getAuthorFeed_memoized/2).

%% memoize_app__bsky__feed__getAuthorFeed_memoized(+Params, -Props).
memoize_app__bsky__feed__getAuthorFeed_memoized(Params, Props) :-
    catch(
        send_request(Params, Pairs, StatusCode),
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
    ),
    assertz(app__bsky__feed__getAuthorFeed_memoized(Params, Props)).

%% app__bsky__feed__getAuthorFeed(+Params, -Props).
%
% [app.bsky.feed.getAuthorFeed](https://docs.bsky.app/docs/api/app-bsky-feed-get-author-feed)
app__bsky__feed__getAuthorFeed(Params, Props) :-
    app__bsky__feed__getAuthorFeed_memoized(Params, Props)
    ->  true
    ;   memoize_app__bsky__feed__getAuthorFeed_memoized(Params, Props).
