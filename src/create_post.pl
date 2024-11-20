:- module(create_post, [
    create_post/2
]).
:- use_module(library(debug)).
:- use_module(library(dcgs)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(uuid)).
:- use_module(configuration, [
    credentials_bluesky_handle/1,
    credential_access_jwt/1,
    temporary_dir/1
]).
:- use_module(create_session, [create_session/2]).
:- use_module(http, [create_record_endpoint/1,read_stream/2]).
:- use_module(logger, [log_info/1,log_error/1]).
:- use_module(serialization, [by_key/3]).

% create_post(+Text,-Body).
create_post(Text, Body) :-
    catch(
        credentials_bluesky_handle(BlueskyHandle),
        missing_credentials(Message),
        log_error([Message])
    ),

    uuidv4_string(UuidStr),

    temporary_dir(TempDir),
    append([TempDir, "/create_post-", UuidStr], TempFile),

    append(["touch ", TempFile, "; date -u '+%Y-%m-%dT%H:%M:%SZ' > ", TempFile], Cmd),
    writeq(Cmd),
    shell(Cmd, GetDateStatus),
    ( GetDateStatus \= 0
    ->  throw(unexpected_command_exit_code('Failed to get date'))
    ;   true ),

    atom_chars(Uuid, UuidStr),
    open(TempFile, read, Stream, [type(text),alias(Uuid)]),
    read_stream(Stream, TimeChars),
    append(["rm ", TempFile], Cmd),
    shell(Cmd, RemoveTempFileStatus),
    ( GetDateStatus \= 0
    ->  throw(unexpected_command_exit_code('Failed to remove temporary file'))
    ;   true ),

    append(["{\"text\": \"", Text, "\", \"createdAt\": \"", TimeChars, "\"}"], Record),
    writeq(Record).
%
%    append(["{\"repo\": \"",BlueskyHandle,"\""], WithBlueskyHandle),
%    append([WithBlueskyHandle, \"collection\": \"app.bsky.feed.post\""], WithCollection),
%    append([WithCollection, \"record\"": Record, "}"], WithRecord),
%    atom_chars(Data, WithRecord),
%
%    atom_chars(Endpoint, EndpointChars),
%
%    catch(
%        credential_access_jwt(AccessJwt),
%        missing_access_jwt(Message),
%        create_session(AccessJwt, _RefreshJwt)
%    ),
%
%    append(['Bearer ', AccessJwt], BearerToken),
%    ListHeaders = [
%        'Content-Type'('application/json'),
%        'Authorization'(BearerToken)
%    ],
%
%    Options = [
%        method(post),
%        status_code(StatusCode),
%        request_headers(ListHeaders),
%        data(Data)
%    ],
%
%    create_record_endpoint(Endpoint),
%    (   http_open(Endpoint, Stream, Options)
%    ->  log_info(['status code: ', StatusCode])
%    ;   throw(cannot_create_session('Failed to create session')) ),
%
%    * read_stream(Stream, BodyChars),
%    * phrase(json_chars(pairs(Pairs)), BodyChars),
%    * atom_chars(Body, Pairs).
