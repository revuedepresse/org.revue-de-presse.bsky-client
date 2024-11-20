:- module(http, [
    read_stream/2,
    create_record_endpoint/1,
    create_session_endpoint/1
]).
:- use_module(configuration, [personal_data_server_host/1]).
:- use_module(library(lists)).

% read_stream(+Stream, -Chars).
read_stream(Stream, []) :-
    at_end_of_stream(Stream).
read_stream(Stream, [Char|Rest]) :-
    get_char(Stream, Char),
    read_stream(Stream, Rest).

% endpoint(+EndpointKind, -Endpoint).
endpoint(EndpointKind, Endpoint) :-
    catch(
        personal_data_server_host(PersonalDataServerHost),
        missing_personal_data_server_host(Message),
        log_error([Message])
    ),
    append(["https://",PersonalDataServerHost,"/xrpc/", EndpointKind], EndpointChars),
    atom_chars(Endpoint, EndpointChars).

% create_record_endpoint(-Endpoint).
create_record_endpoint(Endpoint) :-
    endpoint("com.atproto.server.createRecord", Endpoint).

% create_session_endpoint(-Endpoint).
create_session_endpoint(Endpoint) :-
    endpoint("com.atproto.server.createSession", Endpoint).
