:- module(create_session, [
    create_session/2
]).
:- use_module(library(dcgs)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(configuration, [credentials/2]).
:- use_module(serialization, [by_key/3]).
:- use_module(http, [read_stream/2,create_session_endpoint/1]).
:- use_module(logger, [log_info/1,log_error/1]).

% create_session(-AccessJwt, -RefreshJwt).
create_session(AccessJwt, RefreshJwt) :-
    catch(
        credentials(BlueskyHandle, BlueskyPassword),
        missing_credentials(Message),
        log_error([Message])
    ),

    append(["{\"identifier\": \"", BlueskyHandle, "\",\"password\": \"", BlueskyPassword, "\"}"], DataChars),
    atom_chars(Data, DataChars),

    ListHeaders = ['Content-Type'('application/json')],
    log_info(['headers: ', ListHeaders]),

    Options = [
        method(post),
        status_code(StatusCode),
        request_headers(ListHeaders),
        data(Data)
    ],

    create_session_endpoint(Endpoint),
    log_info(['endpoint: ', Endpoint]),

    (   http_open(Endpoint, Stream, Options)
    ->  log_info(['status code: ', StatusCode])
    ;   throw(cannot_create_session('Failed to create session')) ),

    read_stream(Stream, BodyChars),
    phrase(json_chars(pairs(Pairs)), BodyChars),

    by_key("accessJwt", Pairs, AccessJwtChars),
    atom_chars(AccessJwt, AccessJwtChars),

    by_key("refreshJwt", Pairs, RefreshJwtChars),
    atom_chars(RefreshJwt, RefreshJwtChars).

