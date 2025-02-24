:- module('getProfile', [
    app__bsky__actor__getProfile/2
]).

:- use_module(library(assoc)).
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
    pairs_to_assoc/2
]).
:- use_module('../../../stream', [
    read_stream/2,
    writeln/1,
    writeln/2
]).
:- use_module('../../../string', [concat_as_string/3]).
:- use_module('../../../api', [endpoint_spec_pairs/2]).

%% app__bsky__actor__getProfile_endpoint(+OperationId, +ParamName, +Param, -Endpoint).
app__bsky__actor__getProfile_endpoint(OperationId, ParamName, Param, Endpoint) :-
    public_bluesky_appview_api_endpoint(OperationId, EndpointWithoutParam),
    concat_as_string([EndpointWithoutParam, "?", ParamName, "=", Param], [], Endpoint).

%% app__bsky__actor__getProfile_headers(-ListHeaders).
app__bsky__actor__getProfile_headers(ListHeaders) :-
    header_content_type_application_json(ApplicationJsonContentTypeHeader),

    credentials_access_jwt(AccessJwt),
    append(["Bearer ", AccessJwt], BearerTokenChars),
    atom_chars(BearerToken, BearerTokenChars),

    ListHeaders = [
        ApplicationJsonContentTypeHeader,
        'Authorization'(BearerToken),
        'User-Agent'('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36')
    ].

%% send_request(+ParamValue, -ResponsePairs, -StatusCode).
send_request(ParamValue, ResponsePairs, StatusCode) :-
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

    app__bsky__actor__getProfile_headers(ListHeaders),

    Options = [
        method(VerbAtom),
        status_code(StatusCode),
        request_headers(ListHeaders),
        headers(ResponseHeaders)
    ],

    app__bsky__actor__getProfile_endpoint(OperationId, ParamName, ParamValue, Endpoint),

    catch(
        once((
            writeln(endpoint: Endpoint, true),
            writeln('terra incognita right here ><', true),
            (   http_open(Endpoint, Stream, Options)
            ->  writeln('oopsie', true)
            ;   throw(http_request_failed(Endpoint, Options)) )
        )),
        E,
        (writeln(cannot_submit_http_request(E), true), halt)
    ),


    writeln(response_headers: ResponseHeaders, true),

    % read_stream(Stream, BodyChars),
    get_n_chars(Stream, _N, BodyChars),
    log_info(['body ', BodyChars]),

    phrase(json_chars(pairs(ResponsePairs)), BodyChars),

    append([OperationId, " call failed"], FailedHttpRequestErrorMessage),
    chars_si(FailedHttpRequestErrorMessage),
    atom_chars(FailedHttpRequestErrorMessageAtom, FailedHttpRequestErrorMessage),

    (   StatusCode = 200
    ->  log_debug(['status code: ', StatusCode])
    ;   throw(failed_http_request(FailedHttpRequestErrorMessageAtom, ResponsePairs, StatusCode)) ).

:- dynamic(app__bsky__actor__getProfile_memoized/2).

% memoize_app__bsky__actor__getProfile_memoized(+ParamValue, -Props).
memoize_app__bsky__actor__getProfile_memoized(ParamValue, Props) :-
    catch(
        send_request(ParamValue, Pairs, StatusCode),
        failed_http_request(Message, Pairs, StatusCode),
        log_info([Message])
    ),

    (   StatusCode \= 200
    ->  by_key("message", Pairs, ErrorMessageChars),
        chars_si(ErrorMessageChars),
        atom_chars(ErrorMessage, ErrorMessageChars),
        log_info([ErrorMessage]), fail
    ;   keys(Pairs, [], Keys),
        maplist(writeln, Keys),
        Props = Pairs ),
    assertz(app__bsky__actor__getProfile_memoized(ParamValue, Props)).

%% app__bsky__actor__getProfile(+ParamValue, -Props).
%
% [app.bsky.actor.getProfile](https://docs.bsky.app/docs/api/app-bsky-actor-get-profile)
app__bsky__actor__getProfile(ParamValue, Props) :-
    % endpoint_spec_pairs(SpecPairs, false),
    app__bsky__actor__getProfile_memoized(ParamValue, Props)
    ->  true
    ;   memoize_app__bsky__actor__getProfile_memoized(ParamValue, Props).
