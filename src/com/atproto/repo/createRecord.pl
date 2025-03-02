:- module(createRecord, [
    com__atproto__repo__createRecord/2
]).

:- use_module(library(dcgs)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(serialization/json)).

:- use_module('../../../configuration', [
    credentials_bluesky_handle/1,
    credentials_access_jwt/1,
    log_level/1,
    temporary_dir/1
]).
:- use_module('../server/createSession', [com__atproto__server__createSession/2]).
:- use_module('../../../http', [
    personal_data_server_endpoint/2,
    header_content_type_application_json/1
]).
:- use_module('../../../logger', [
    log_debug/1,
    log_error/1,
    log_info/1
]).
:- use_module('../../../serialization', [by_key/3]).
:- use_module('../../../stream', [
    read_stream/2,
    read_stream/3
]).
:- use_module('../../../temporal', [date_iso8601/1]).

% com__atproto__repo__createRecord_endpoint(-Endpoint).
com__atproto__repo__createRecord_endpoint(Endpoint) :-
    personal_data_server_endpoint("com.atproto.repo.createRecord", Endpoint).

% com__atproto__repo__createRecord_headers(-ListHeaders).
com__atproto__repo__createRecord_headers(ListHeaders) :-
    log_level(LogLevel),
    unsetenv("LOG_LEVEL"),
    catch(
        credentials_access_jwt(AccessJwt),
        empty_env_var_value(_Message),
        com__atproto__server__createSession(AccessJwt, _RefreshJwt)
    ),
    setenv("LOG_LEVEL", LogLevel),

    append(["Bearer ", AccessJwt], BearerTokenChars),
    atom_chars(BearerToken, BearerTokenChars),

    header_content_type_application_json(ApplicationJsonContentTypeHeader),
    ListHeaders = [
        ApplicationJsonContentTypeHeader,
        'Authorization'(BearerToken)
    ].

:- dynamic(com__atproto__repo__createRecord_memoized/2).

% send_request(+Text, -Pairs, -StatusCode).
send_request(Text, Pairs, StatusCode) :-
    catch(
        credentials_bluesky_handle(BlueskyHandle),
        missing_credentials(Message),
        log_error([Message])
    ),

    date_iso8601(Date),

    append(["{ \"text\": \"", Text, "\", \"createdAt\": \"", Date, "\" }"], RecordChars),

    append(["{ \"repo\": \"", BlueskyHandle, "\", "], WithBlueskyHandle),
    append([WithBlueskyHandle, "\"collection\": \"app.bsky.feed.post\", "], WithCollection),
    append([WithCollection, "\"record\": ", RecordChars, " }"], WithRecord),

    atom_chars(Data, WithRecord),
    log_info(['data: ', Data]),

    com__atproto__repo__createRecord_headers(ListHeaders),

    Options = [
        method(post),
        status_code(StatusCode),
        request_headers(ListHeaders),
        data(Data),
        headers(_ResponseHeaders)
    ],

    com__atproto__repo__createRecord_endpoint(Endpoint),

    once(http_open(Endpoint, Stream, Options)),
    log_debug(Options),

    read_stream(Stream, BodyChars),

    phrase(json_chars(pairs(Pairs)), BodyChars),

    (   StatusCode = 200
    ->  writeln(status_code(StatusCode), true)
    ;   throw(failed_http_request('com.atproto.repo.createRecord call failed', Pairs, StatusCode)) ).

% memoize_com__atproto__repo__createRecord_response(+Text, -Props).
memoize_com__atproto__repo__createRecord_response(Text, Props) :-
    catch(
        send_request(Text, Pairs, StatusCode),
        failed_http_request(Message, Pairs, StatusCode),
        log_info([Message])
    ),

    (   StatusCode \= 200
    ->  by_key("message", Pairs, ErrorMessageChars),
        atom_chars(ErrorMessage, ErrorMessageChars),
        log_info([ErrorMessage]), fail
    ;   by_key("cid", Pairs, CidChars),
        atom_chars(Cid, CidChars),
        by_key("uri", Pairs, UriChars),
        atom_chars(Uri, UriChars),
        by_key("validationStatus", Pairs, ValidationStatusChars),
        atom_chars(ValidationStatus, ValidationStatusChars),
        by_key("commit", Pairs, CommitPairs),
        by_key("cid", CommitPairs, CommitCid),
        by_key("rev", CommitPairs, Rev),
        Props = [Cid-Uri-ValidationStatus-CommitCid-Rev] ),
    assertz(com__atproto__repo__createRecord_memoized(Text, Props)).

% com__atproto__repo__createRecord(+Text, -Props).
com__atproto__repo__createRecord(Text, Props) :-
    com__atproto__repo__createRecord_memoized(Text, Props)
    ->  true
    ;   memoize_com__atproto__repo__createRecord_response(Text, Props).
