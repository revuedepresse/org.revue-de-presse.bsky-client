:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(http/http_open)).
:- use_module(library(os)).

log_info([]) :- nl.
log_info([Message|Rest]) :-
    writeq(Message),
    log_info(Rest).

log_error(Message) :-
    write(Message),nl,fail.

credentials(BlueskyHandle, BlueskyPassword) :-
    (   getenv("BLUESKY_HANDLE", BlueskyHandle)
    ->  true
    ;   throw(invalid_credentials('Please export BLUESKY_HANDLE environment variable.')) ),
    (   getenv("BLUESKY_PASSWORD", BlueskyPassword)
    ->  true
    ;   throw(invalid_credentials('Please export BLUESKY_PASSWORD environment variable.')) ).

create_session(Stream) :-
    catch(
        credentials(BlueskyHandle, BlueskyPassword),
        invalid_credentials(Message),
        log_error(Message)
    ),

    append(["{\"identifier\": \"",BlueskyHandle,"\",\"password\": \"", BlueskyPassword, "\"}"], DataChars),
    atom_chars(Data, DataChars),

    Endpoint = 'https://bsky.social/xrpc/com.atproto.server.createSession',
    ListHeaders = ['Content-Type'('application/json')],

    log_info(['data: ', Data]),
    log_info(['endpoint: ', Endpoint]),
    log_info(['headers: ', ListHeaders]),

    Options = [method(post),status_code(StatusCode),request_headers(ListHeaders),data(Data)],
    (   http_open(Endpoint, Stream, Options)
    ->  log_info(['status code: ', StatusCode])
    ;   log_error(['Failed to create session.']),
        throw(cannot_create_session)
    ).

