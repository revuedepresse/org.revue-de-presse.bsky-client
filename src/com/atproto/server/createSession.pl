:- module(createSession, [
    com__atproto__server__createSession/2
]).

:- use_module(library(dcgs)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(serialization/json)).

:- use_module('../../../configuration', [credentials/2]).
:- use_module('../../../serialization', [by_key/3]).
:- use_module('../../../http', [
    personal_data_server_endpoint/2,
    header_content_type_application_json/1
]).
:- use_module('../../../logger', [
    log_debug/1,
    log_info/1,
    log_error/1
]).
:- use_module('../../../stream', [read_stream/3]).

% com__atproto__server__createSession_endpoint(-Endpoint).
com__atproto__server__createSession_endpoint(Endpoint) :-
    personal_data_server_endpoint("com.atproto.server.createSession", Endpoint).

:- dynamic(com__atproto__server__createSession_memoized/2).

% memoize_jw_tokens(-AccessJwt, -RefreshJwt).
memoize_jw_tokens(AccessJwt, RefreshJwt) :-
    catch(
        credentials(BlueskyHandle, BlueskyPassword),
        missing_credentials(Message),
        log_error([Message])
    ),

    append(["{\"identifier\": \"", BlueskyHandle, "\",\"password\": \"", BlueskyPassword, "\"}"], DataChars),
    atom_chars(Data, DataChars),

    header_content_type_application_json(ApplicationJsonContentTypeHeader),
    ListHeaders = [ApplicationJsonContentTypeHeader],

    Options = [
        method(post),
        status_code(StatusCode),
        request_headers(ListHeaders),
        data(Data),
        headers(ResponseHeaders)
    ],

    com__atproto__server__createSession_endpoint(Endpoint),
    log_debug(['http_open options: ', Options]),

    http_open(Endpoint, Stream, Options), !,
    log_debug(['Response headers: ', ResponseHeaders]),

    (   StatusCode = 200
    ->  log_info(['status code: ', StatusCode])
    ;   throw(cannot_create_session('Failed to create session', StatusCode)) ),

    read_stream(Stream, [], BodyChars),
    phrase(json_chars(pairs(Pairs)), BodyChars),

    by_key("accessJwt", Pairs, AccessJwt),
    by_key("refreshJwt", Pairs, RefreshJwt),
    assertz(com__atproto__server__createSession_memoized(AccessJwt, RefreshJwt)).

% com__atproto__server__createSession(-AccessJwt, -RefreshJwt).
com__atproto__server__createSession(AccessJwt, RefreshJwt) :-
    com__atproto__server__createSession_memoized(AccessJwt, RefreshJwt)
    ->  true
    ;   memoize_jw_tokens(AccessJwt, RefreshJwt).

