:- module('getLists', [
    app__bsky__graph__getLists/2
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(reif)).
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
:- use_module('../../../parameters', [
    extract_single_required_parameter/2
]).
:- use_module('../../../serialization', [
    by_key/3,
    keys/3,
    pairs_to_assoc/2
]).
:- use_module('../../../stream', [
    writeln/1,
    writeln/2
]).
:- use_module('../../../string', [concat_as_string/3]).
:- use_module('../../../api', [endpoint_spec_pairs/2]).

%% app__bsky__graph__getLists_endpoint(+OperationId, +ParamName, +Param, -Endpoint).
app__bsky__graph__getLists_endpoint(OperationId, ParamName, Param, Endpoint) :-
    public_bluesky_appview_api_endpoint(OperationId, EndpointWithoutParam),
    concat_as_string([EndpointWithoutParam, "?", ParamName, "=", Param], [], Endpoint).

%% app__bsky__graph__getLists_headers(-ListHeaders).
app__bsky__graph__getLists_headers(ListHeaders) :-
    header_content_type_application_json(ApplicationJsonContentTypeHeader),
    ListHeaders = [ApplicationJsonContentTypeHeader].

%% send_request(+Actor, -ResponsePairs, -StatusCode).
send_request(Actor, ResponsePairs, StatusCode) :-
    endpoint_spec_pairs(SpecPairs, false),

    pairs_keys(SpecPairs, [string(Verb)]),
    pairs_to_assoc(SpecPairs, SpecAssoc),

    atom_chars(VerbAtom, Verb),
    get_assoc(VerbAtom, SpecAssoc, EndpointSpecAssoc),
    get_assoc(parameters, EndpointSpecAssoc, Parameters),
    get_assoc(operationId, EndpointSpecAssoc, OperationId),

    extract_single_required_parameter(Parameters, RequiredParameter),
    get_assoc(name, RequiredParameter, ParamName),
    get_assoc(in, RequiredParameter, Where),

    once((
        Where = "query"
    ;   throw(error_unsupported_parameter(Where)) )),

    app__bsky__graph__getLists_headers(ListHeaders),

    Options = [
        method(VerbAtom),
        status_code(StatusCode),
        request_headers(ListHeaders),
        headers(_ResponseHeaders)
    ],

    app__bsky__graph__getLists_endpoint(OperationId, ParamName, Actor, Endpoint),

    http_open(Endpoint, Stream, Options), !,
    log_debug(Options),

    get_n_chars(Stream, _, BodyChars),
    log_debug(['body ', BodyChars]),

    phrase(json_chars(pairs(ResponsePairs)), BodyChars),

    append([OperationId, " call failed"], FailedHttpRequestErrorMessage),
    atom_chars(FailedHttpRequestErrorMessageAtom, FailedHttpRequestErrorMessage),

    if_(
        StatusCode = 200,
        writeln('status code':StatusCode, true),
        throw(failed_http_request(FailedHttpRequestErrorMessageAtom, ResponsePairs, StatusCode))
    ).

:- dynamic(app__bsky__graph__getLists_memoized/2).

%% memoize_app__bsky__graph__getLists_memoized(+Actor, -Props).
memoize_app__bsky__graph__getLists_memoized(Actor, Props) :-
    catch(
        send_request(Actor, Pairs, StatusCode),
        failed_http_request(Message, Pairs, StatusCode),
        log_info([Message])
    ),

    if_(
        dif(StatusCode, 200),
       (by_key("message", Pairs, ErrorMessageChars),
        atom_chars(ErrorMessage, ErrorMessageChars),
        log_error([ErrorMessage]), fail),
        Props = Pairs
    ),
    assertz(app__bsky__graph__getLists_memoized(Actor, Props)).

%% [app.bsky.graph.getLists](https://docs.bsky.app/docs/api/app-bsky-graph-get-lists)
%%
%% app__bsky__graph__getLists(+Actor, -Props).
app__bsky__graph__getLists(Actor, Props) :-
    app__bsky__graph__getLists_memoized(Actor, Props)
    ->  true
    ;   memoize_app__bsky__graph__getLists_memoized(Actor, Props).
