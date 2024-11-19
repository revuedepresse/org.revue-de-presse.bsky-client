:- module('getProfile', [
    app__bsky__actor__getProfile/2
]).

:- use_module(library(debug)).
:- use_module(library(dcgs)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(serialization/json)).

:- use_module('../../../http', [
    public_bluesky_appview_api_endpoint/2,
    header_content_type_application_json/1
]).
:- use_module('../../../logger', [
    log_debug/1,
    log_error/1,
    log_info/1
]).
:- use_module('../../../serialization', [
    by_key/3,
    keys/3
]).
:- use_module('../../../stream', [
    read_stream/3,
    writeln/1
]).
:- use_module('../../../string', [concat_as_string/3]).

% app__bsky__actor__getProfile_endpoint(+Actor, -Endpoint).
app__bsky__actor__getProfile_endpoint(Actor, Endpoint) :-
    public_bluesky_appview_api_endpoint("app.bsky.actor.getProfile", EndpointWithoutActor),
    concat_as_string([EndpointWithoutActor, "?actor=", Actor], [], Endpoint).

% get_profile_headers(-ListHeaders).
app__bsky__getProfile_headers(ListHeaders) :-
    header_content_type_application_json(ApplicationJsonContentTypeHeader),
    ListHeaders = [ApplicationJsonContentTypeHeader].

:- dynamic(app__bsky__actor__getProfile_memoized/2).

% send_request(+Actor, -Pairs, -StatusCode).
send_request(Actor, Pairs, StatusCode) :-
   app__bsky__getProfile_headers(ListHeaders),

    Options = [
        method(get),
        status_code(StatusCode),
        request_headers(ListHeaders),
        headers(_ResponseHeaders)
    ],

    app__bsky__actor__getProfile_endpoint(Actor, Endpoint),

    http_open(Endpoint, Stream, Options), !,
    log_debug(Options),

    read_stream(Stream, [], BodyChars),
    log_info(['body ', BodyChars]),

    phrase(json_chars(pairs(Pairs)), BodyChars),

    (   StatusCode = 200
    ->  log_info(['status code: ', StatusCode])
    ;   throw(failed_http_request('app.bsky.actor.getProfile call failed', Pairs, StatusCode)) ).

% memoize_create_post_response(+Actor, -Props).
memoize_app__bsky__actor__getProfile_memoized(Actor, Props) :-
    catch(
        send_request(Actor, Pairs, StatusCode),
        failed_http_request(Message, Pairs, StatusCode),
        log_info([Message])
    ),

    (   StatusCode \= 200
    ->  by_key("message", Pairs, ErrorMessageChars),
        atom_chars(ErrorMessage, ErrorMessageChars),
        log_info([ErrorMessage]), fail
    ;   keys(Pairs, [], Keys),
        maplist(writeln, Keys),
        Props = [] ),
    assertz(app__bsky__actor__getProfile_memoized(Actor, Props)).

% [app.bsky.actor.getProfile](https://docs.bsky.app/docs/api/app-bsky-actor-get-profile)
%
% app__bsky__actor__getProfile(+Actor, -Props).
app__bsky__actor__getProfile(Actor, Props) :-
    app__bsky__actor__getProfile_memoized(Actor, Props)
    ->  true
    ;   memoize_app__bsky__actor__getProfile_memoized(Actor, Props).
