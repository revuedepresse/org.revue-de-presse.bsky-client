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
:- use_module('../../../app/bsky/feed/getAuthorFeed', [
    app__bsky__feed__getAuthorFeed/2
]).
:- use_module('../../../domain/events/app/bsky/actor/event_getProfile', [
    onGetProfile/2
]).
:- use_module('../../../domain/events/app/bsky/graph/event_getList', [
    onGetList/3
]).
:- use_module('../../../domain/events/app/bsky/graph/event_getListItem', [
    onGetListItem/3
]).
:- use_module('../../../infrastructure/pg/connection', [
    open_pg_connection/1,
    close_pg_connection/1
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
    concat_as_string([EndpointWithoutParam, "?limit=50&", ParamName, "=", Param], [], Endpoint).

%% app__bsky__graph__getList_headers(-ListHeaders).
app__bsky__graph__getList_headers(ListHeaders) :-
    header_content_type_application_json(ApplicationJsonContentTypeHeader),
    ListHeaders = [ApplicationJsonContentTypeHeader].

%% send_request(+Session, +MainListAtUri, -ResponsePairs, -StatusCode).
send_request(pg_session(C0, C), MainListAtUri, ResponsePairs, StatusCode) :-
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
        headers(_ResponseHeaders)
    ],

    app__bsky__graph__getList_endpoint(OperationId, ParamName, MainListAtUri, Endpoint),
    once(http_open(Endpoint, Stream, Options)),

    read_stream(Stream, BodyChars),
    phrase(json_chars(pairs(ResponsePairs)), BodyChars),
    pairs_to_assoc(ResponsePairs, JSONAssoc),
    get_assoc(list, JSONAssoc, List),
    get_assoc(uri, List, ListUri),

    payload(BodyChars, Payload),

    % See ./domain/.../graph/event_getList.pl
    % where `onGetList` event handler is implemented
    onGetList(pg_session(C0, C1), ListUri, Payload),

    % Emits
    % - `onGetListItem` event handled by ./domain/.../graph/event_getListsItem.pl,
    % - `onGetProfile` event handled by ./domain/.../actor/event_getProfile.pl
    list_uri(pg_session(C1, C), MainListAtUri, JSONAssoc, ListUri),

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

%% list_uri(+Session, +MainListAtUri, +JSONAssoc, -Uri).
list_uri(Session, MainListAtUri, JSONAssoc, Uri) :-
    get_assoc(list, JSONAssoc, List),
    get_assoc(items, JSONAssoc, ReversedItems),
    reverse(ReversedItems, Items),
    process_items(Session, MainListAtUri, Items),
    get_assoc(uri, List, Uri).

    %% process_items(+Session, +_MainListAtUri, +ItemsAssocs).
    %
    % Folds the connection through every item, first for the
    % profiles (publishers_list rows), then for the list items
    % (profile events and weaving_user records).
    process_items(pg_session(C0, C), MainListAtUri, ItemsAssocs) :-
        maplist(wrapped_pairs_to_assoc, ItemsAssocs, WrappedItemsAssocs),
        foldl(try_to_get_profile(MainListAtUri), WrappedItemsAssocs, C0, C1),
        foldl(on_get_list_item, WrappedItemsAssocs, ItemsAssocs, C1, C).

    %% on_get_list_item(+WrappedItemAssoc, +ItemAssoc, +In, -Out).
    on_get_list_item(WrappedItemAssoc, ItemAssoc, In, Out) :-
        onGetListItem(pg_session(In, Out), WrappedItemAssoc, ItemAssoc).

    %% try_to_get_profile(+MainListAtUri, +Assoc, +In, -Out).
    %
    % The catch undoes the bindings of the goal it guards, so a
    % caught error leaves the session on its input connection.
    try_to_get_profile(MainListAtUri, Assoc, In, Out) :-
        catch(
            get_profile(MainListAtUri, Assoc, In, Out),
            E,
            ( Out = In,
              if_(
                  E = pre_existing_publisher(_),
                  writeln(cannot_select_publishers(E), true),
                  log_error([error_on_getProfile_event(E)])
              ) )
        ).

    %% get_profile(+MainListAtUri, +Assoc, +In, -Out).
    get_profile(MainListAtUri, Assoc, In, Out) :-
        get_assoc(subject, Assoc, Subject),
        get_assoc(did, Subject, ActorParam),

        writeln(gettingProfileByDID(ActorParam)),
        app__bsky__actor__getProfile(ActorParam, Payload),
        app__bsky__feed__getAuthorFeed(ActorParam, _),

        writeln(received_payload(Payload)),
        pairs_to_assoc(Payload, PayloadAssoc),

        get_assoc(followersCount, PayloadAssoc, FollowersCount),
        get_assoc(followsCount, PayloadAssoc, FollowsCount),

        onGetProfile(pg_session(In, Out), props(MainListAtUri, FollowersCount, FollowsCount, ActorParam, Payload)).

%% [app.bsky.graph.getList](https://docs.bsky.app/docs/api/app-bsky-graph-get-list)
%
%% app__bsky__graph__getList(+MainListAtUri, -Props).
%
% Opens one connection for the whole list and closes it on every
% path, as app__bsky__feed__getAuthorFeed/2 does. A request that
% fails without throwing is turned into a throw so the connection
% is closed before the predicate fails.
app__bsky__graph__getList(MainListAtUri, Props) :-
    open_pg_connection(Conn0),
    catch(
        ( once((
              send_request(pg_session(Conn0, ConnFinal), MainListAtUri, Pairs, StatusCode)
          ;   throw(get_list_request_failed(MainListAtUri))
          )),
          close_session(Conn0, ConnFinal) ),
        E,
        ( best_effort_close_session(Conn0, ConnFinal),
          if_(
              E = failed_http_request(Message, Pairs, StatusCode),
              (log_error([Message]), fail),
              (log_error([E]), fail)
          ) )
    ),

    if_(
        dif(StatusCode, 200),
        (by_key("message", Pairs, ErrorMessageChars),
        chars_si(ErrorMessageChars),
        atom_chars(ErrorMessage, ErrorMessageChars),
        log_error([ErrorMessage]), fail),
        Props = Pairs
    ).

%% close_session(+Conn0, +ConnFinal)
close_session(Conn0, ConnFinal) :-
    close_pg_connection(ConnFinal),
    close_pg_connection_if_distinct(Conn0, ConnFinal).

%% best_effort_close_session(+Conn0, ?ConnFinal)
%
% Same contract as getAuthorFeed's: after a throw ConnFinal may be
% unbound, in which case only Conn0 is closed. No cuts.
best_effort_close_session(Conn0, ConnFinal) :-
    nonvar(ConnFinal),
    close_session(Conn0, ConnFinal).
best_effort_close_session(Conn0, ConnFinal) :-
    var(ConnFinal),
    close_pg_connection(Conn0).

close_pg_connection_if_distinct(Conn0, Conn0).
close_pg_connection_if_distinct(Conn0, ConnFinal) :-
    dif(Conn0, ConnFinal),
    close_pg_connection(Conn0).
