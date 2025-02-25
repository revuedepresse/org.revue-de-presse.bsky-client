:- module('getList', [
    app__bsky__graph__getList/2
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(reif)).
:- use_module(library(si)).
:- use_module(library(serialization/json)).
:- use_module(library(time)).

:- use_module('../../../app/bsky/actor/getProfile', [
    app__bsky__actor__getProfile/2
]).
:- use_module('../../../domain/events/app/bsky/actor/event_getProfile', [
    onGetProfile/1
]).
:- use_module('../../../domain/events/app/bsky/graph/event_getList', [
    onGetList/2
]).
:- use_module('../../../domain/events/app/bsky/graph/event_getListItem', [
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
    writeln/1,
    writeln/2
]).
:- use_module('../../../string', [concat_as_string/3]).
:- use_module('../../../api', [endpoint_spec_pairs/2]).

%% app__bsky__graph__getList_endpoint(+OperationId, +ParamName, +Param, -Endpoint).
app__bsky__graph__getList_endpoint(OperationId, ParamName, Param, Endpoint) :-
    public_bluesky_appview_api_endpoint(OperationId, EndpointWithoutParam),
    concat_as_string([EndpointWithoutParam, "?limit=50&", ParamName, "=", Param], [], Endpoint).

%% app__bsky__graph__getList_headers(-ListHeaders).
app__bsky__graph__getList_headers(ListHeaders) :-
    header_content_type_application_json(ApplicationJsonContentTypeHeader),
    ListHeaders = [ApplicationJsonContentTypeHeader].

%% send_request(+MainListAtUri, -ResponsePairs, -StatusCode).
send_request(MainListAtUri, ResponsePairs, StatusCode) :-
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
    get_assoc(in, RequiredParameter, Where),

    once((
        Where = "query"
    ;   throw(error_unsupported_parameter(Where)) )),

    app__bsky__graph__getList_headers(ListHeaders),

    Options = [
        method(VerbAtom),
        status_code(StatusCode),
        request_headers(ListHeaders),
        headers(ResponseHeaders)
    ],

    app__bsky__graph__getList_endpoint(OperationId, ParamName, MainListAtUri, Endpoint),

    http_open(Endpoint, Stream, Options),
    log_debug(Options), !,
    writeln(response_headers: ResponseHeaders),

    get_n_chars(Stream, _, BodyChars),
    phrase(json_chars(pairs(ResponsePairs)), BodyChars),
    pairs_to_assoc(ResponsePairs, JSONAssoc),
    get_assoc(list, JSONAssoc, List),
    get_assoc(uri, List, ListUri),

    payload(BodyChars, Payload),

    % See ./domain/.../graph/event_getList.pl
    % where `onGetList` event handler is implemented
    onGetList(ListUri, Payload),

    % Emits
    % - `onGetListItem` event handled by ./domain/.../graph/event_getListsItem.pl,
    % - `onGetProfile` event handled by ./domain/.../actor/event_getProfile.pl
    list_uri(MainListAtUri, JSONAssoc, ListUri),

    if_(
        dif(StatusCode, 200),
       (append([OperationId, " call failed"], FailedHttpRequestErrorMessage),
        chars_si(FailedHttpRequestErrorMessage),
        atom_chars(FailedHttpRequestErrorMessageAtom, FailedHttpRequestErrorMessage),
        log_error(['status code: ', StatusCode]),
        throw(failed_http_request(
            FailedHttpRequestErrorMessageAtom, ResponsePairs, StatusCode)
        )),
        writeln('getList status code': StatusCode, true)
    ).

%% payload(+BodyChars, -Payload).
payload(BodyChars, Payload) :-
    to_json_chars(BodyChars, JSONChars),
    write_term_to_chars(JSONChars, [quoted(false),double_quotes(true)], Chars),
    chars_utf8bytes(Chars, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, Utf8BytesPayload),
    chars_base64(Utf8BytesPayload, Payload, []).

%% list_uri(+MainListAtUri, +JSONAssoc, -Uri).
list_uri(MainListAtUri, JSONAssoc, Uri) :-
    get_assoc(list, JSONAssoc, List),
    get_assoc(items, JSONAssoc, Items),
    process_items(MainListAtUri, Items),
    get_assoc(uri, List, Uri).

    %% process_items(+_MainListAtUri, +ItemsAssocs).
    process_items(MainListAtUri, ItemsAssocs) :-
        maplist(wrapped_pairs_to_assoc, ItemsAssocs, WrappedItemsAssocs),
        foldl(try_to_get_profile(MainListAtUri), WrappedItemsAssocs, [], []),
        maplist(onGetListItem, WrappedItemsAssocs, ItemsAssocs).

    %% try_to_get_profile(+MainListAtUri, +Assoc, +In, -In).
    try_to_get_profile(MainListAtUri, Assoc, In, In) :-
        catch(
            get_profile(MainListAtUri, Assoc, In, In),
            E,
            if_(
                E = pre_existing_publisher(_),
                writeln(cannot_select_publishers(E), true),
                log_error([error_on_getProfile_event(E)])
            )
        ).

    %% get_profile(+MainListAtUri, +Assoc, +In, -In).
    get_profile(MainListAtUri, Assoc, In, In) :-
        get_assoc(subject, Assoc, Subject),
        get_assoc(did, Subject, ActorParam),

        writeln(gettingProfileByDID(ActorParam)),
        app__bsky__actor__getProfile(ActorParam, Payload),

        % [Content Write Operations (per account)](https://docs.bsky.app/docs/advanced-guides/rate-limits#content-write-operations-per-account)
        TimeToWaitInSecBeforeNextWriteOperation is ceiling(5000/3600),
        sleep(TimeToWaitInSecBeforeNextWriteOperation),

        writeln(received_payload(Payload)),

        pairs_to_assoc(Payload, PayloadAssoc),

        get_assoc(followersCount, PayloadAssoc, FollowersCount),
        get_assoc(followsCount, PayloadAssoc, FollowsCount),

        onGetProfile(props(MainListAtUri, FollowersCount, FollowsCount, ActorParam, Payload)).

:- dynamic(app__bsky__graph__getList_memoized/2).

%% memoize_app__bsky__graph__getList_memoized(+MainListAtUri, -Props).
memoize_app__bsky__graph__getList_memoized(MainListAtUri, Props) :-
    catch(
        (send_request(MainListAtUri, Pairs, StatusCode)),
        E,
        if_(
            E = failed_http_request(Message, Pairs, StatusCode),
            (log_error([Message]), fail),
            (log_error([E]), fail)
        )
    ),

    if_(
        dif(StatusCode, 200),
        (by_key("message", Pairs, ErrorMessageChars),
        chars_si(ErrorMessageChars),
        atom_chars(ErrorMessage, ErrorMessageChars),
        log_error([ErrorMessage]), fail),
        Props = Pairs
    ),
    assertz(app__bsky__graph__getList_memoized(MainListAtUri, Props)).

%% [app.bsky.graph.getList](https://docs.bsky.app/docs/api/app-bsky-graph-get-list)
%
%% app__bsky__graph__getList(+MainListAtUri, -Props).
app__bsky__graph__getList(MainListAtUri, Props) :-
    app__bsky__graph__getList_memoized(MainListAtUri, Props)
    ->  true
    ;   memoize_app__bsky__graph__getList_memoized(MainListAtUri, Props).
