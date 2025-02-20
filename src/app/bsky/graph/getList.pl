:- module('getList', [
    app__bsky__graph__getList/2
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(reif)).
:- use_module(library(serialization/json)).

:- use_module('../../../domain/events/app/bsky/graph/event_getList', [
    onGetList/2,
    onGetListItem/2
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
    extract_single_required_parameter/2
]).
:- use_module('../../../serialization', [
    by_key/3,
    char_code_at/2,
    keys/3,
    pairs_to_assoc/2,
    to_json_chars/2,
    unwrap_pairs/2,
    wrapped_pairs_to_assoc/2
]).
:- use_module('../../../stream', [
    read_stream/2,
    writeln/1,
    writeln/2
]).
:- use_module('../../../string', [concat_as_string/3]).
:- use_module('../../../api', [endpoint_spec_pairs/2]).

%% app__bsky__graph__getList_endpoint(+OperationId, +ParamName, +Param, -Endpoint).
app__bsky__graph__getList_endpoint(OperationId, ParamName, Param, Endpoint) :-
    public_bluesky_appview_api_endpoint(OperationId, EndpointWithoutParam),
    concat_as_string([EndpointWithoutParam, "?", ParamName, "=", Param], [], Endpoint).

%% app__bsky__graph__getList_headers(-ListHeaders).
app__bsky__graph__getList_headers(ListHeaders) :-
    header_content_type_application_json(ApplicationJsonContentTypeHeader),
    ListHeaders = [ApplicationJsonContentTypeHeader].

%% send_request(+MainListAtUri, -ResponsePairs, -StatusCode).
send_request(MainListAtUri, ResponsePairs, StatusCode) :-
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

    app__bsky__graph__getList_headers(ListHeaders),

    Options = [
        method(VerbAtom),
        status_code(StatusCode),
        request_headers(ListHeaders),
        headers(_ResponseHeaders)
    ],

    app__bsky__graph__getList_endpoint(OperationId, ParamName, MainListAtUri, Endpoint),

    http_open(Endpoint, Stream, Options),
    log_debug(Options), !,

    read_stream(Stream, BodyChars),
    phrase(json_chars(pairs(ResponsePairs)), BodyChars),

    payload(BodyChars, Payload),
    list_uri(ResponsePairs, ListUri),

    % See ./domain/events/app/bsky/graph/event_getList.pl
    % where `onGetList` event handler is implemented
    onGetList(ListUri, Payload),

    append([OperationId, " call failed"], FailedHttpRequestErrorMessage),
    atom_chars(FailedHttpRequestErrorMessageAtom, FailedHttpRequestErrorMessage),

    if_(
        dif(StatusCode, 200),
        (log_error(['status code: ', StatusCode]),
        throw(failed_http_request(
            FailedHttpRequestErrorMessageAtom, ResponsePairs, StatusCode)
        )),
        log_debug(['status code: ', StatusCode])
    ).

%% payload(+BodyChars, -Payload).
payload(BodyChars, Payload) :-
    to_json_chars(BodyChars, JSONChars),
    write_term_to_chars(JSONChars, [quoted(false),double_quotes(true)], Chars),
    chars_utf8bytes(Chars, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, Utf8BytesPayload),
    chars_base64(Utf8BytesPayload, Payload, []).

%% list_uri(+ResponsePairs, -Uri).
list_uri(ResponsePairs, Uri) :-
    pairs_to_assoc(ResponsePairs, JSONAssoc),
    get_assoc(list, JSONAssoc, List),
    get_assoc(items, JSONAssoc, Items),
    maplist(wrapped_pairs_to_assoc, Items, ItemsAssocs),
    maplist(onGetListItem, ItemsAssocs, Items),
    get_assoc(uri, List, Uri).

:- dynamic(app__bsky__graph__getList_memoized/2).

%% memoize_app__bsky__graph__getList_memoized(+MainListAtUri, -Props).
memoize_app__bsky__graph__getList_memoized(MainListAtUri, Props) :-
    catch(
        send_request(MainListAtUri, Pairs, StatusCode),
        failed_http_request(Message, Pairs, StatusCode),
        log_info([Message])
    ),

    if_(
        dif(StatusCode, 200),
        (by_key("message", Pairs, ErrorMessageChars),
        atom_chars(ErrorMessage, ErrorMessageChars),
        log_info([ErrorMessage]), fail),
        (keys(Pairs, [], Keys),
        maplist(writeln, Keys),
        Props = [] )
    ),
    assertz(app__bsky__graph__getList_memoized(MainListAtUri, Props)).

%% [app.bsky.graph.getList](https://docs.bsky.app/docs/api/app-bsky-graph-get-list)
%
%% app__bsky__graph__getList(+MainListAtUri, -Props).
app__bsky__graph__getList(MainListAtUri, Props) :-
    app__bsky__graph__getList_memoized(MainListAtUri, Props)
    ->  true
    ;   memoize_app__bsky__graph__getList_memoized(MainListAtUri, Props).
