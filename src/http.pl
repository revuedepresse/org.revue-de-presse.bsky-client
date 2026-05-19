:- module(http, [
    personal_data_server_endpoint/2,
    public_bluesky_appview_api_endpoint/2,
    header_content_type_application_json/1
]).

/**
Helpers for building XRPC endpoint URLs and standard headers.

The Bluesky/atproto API splits traffic between a Personal Data
Server (PDS, the user's home node) and the public AppView (the
read-only aggregator). The two endpoint builders here resolve
the host name from `configuration.pl` and concatenate it with
`/xrpc/<method-id>` to produce a ready-to-fetch atom URL.
*/
:- use_module(configuration, [
    personal_data_server_host/1,
    public_bluesky_app_view_api_host/1
]).
:- use_module(logger, [log_error/1]).
:- use_module(library(lists)).

%% header_content_type_application_json(-Header)
%
% The canonical `Content-Type: application/json` request header.
header_content_type_application_json(Header) :-
    Header = 'Content-Type'('application/json').

% endpoint(+EndpointKind, +Hostmame, -Endpoint).
endpoint(EndpointKind, Hostmame, Endpoint) :-
    append(["https://",Hostmame,"/xrpc/", EndpointKind], EndpointChars),
    atom_chars(Endpoint, EndpointChars).

%% personal_data_server_endpoint(+EndpointKind, -Endpoint)
%
% Build `https://<PDS>/xrpc/<EndpointKind>` as an atom from the
% host returned by `configuration:personal_data_server_host/1`.
personal_data_server_endpoint(EndpointKind, Endpoint) :-
    catch(
        personal_data_server_host(PersonalDataServerHost),
        missing_personal_data_server_host(Message),
        log_error([Message])
    ),
    endpoint(EndpointKind, PersonalDataServerHost, Endpoint).

%% public_bluesky_appview_api_endpoint(+EndpointKind, -Endpoint)
%
% Build `https://<AppView>/xrpc/<EndpointKind>` as an atom from
% `configuration:public_bluesky_app_view_api_host/1`.
public_bluesky_appview_api_endpoint(EndpointKind, Endpoint) :-
    catch(
        public_bluesky_app_view_api_host(PublicBlueskyAppViewApiHost),
        missing_public_bluesky_app_view_api_host(Message),
        log_error([Message])
    ),
    endpoint(EndpointKind, PublicBlueskyAppViewApiHost, Endpoint).
