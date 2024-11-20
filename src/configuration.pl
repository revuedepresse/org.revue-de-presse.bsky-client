:- module(configuration, [
    credentials/2,
    credentials_bluesky_handle/1,
    credentials_access_jwt/1,
    credentials_refresh_jwt/1,
    personal_data_server_host/1,
    temporary_dir/1
]).
:- use_module(library(os)).
:- use_module(library(serialization/json)).

% get_env_or_throw(+Name, -Value, +Error).
get_env_or_throw(Name, Value, Error) :-
    (   getenv(Name, Value)
    ->  true
    ;   throw(Error) ).

% personal_data_server_host(-PersonalDataServerHost).
personal_data_server_host(PersonalDataServerHost) :-
    get_env_or_throw("PDSHOST", PersonalDataServerHost, missing_personal_data_server_host('Please export PDSHOST environment variable.')).

% credentials(-BlueskyHandle, -BlueskyPassword).
credentials(BlueskyHandle, BlueskyPassword) :-
    credentials_bluesky_handle(BlueskyHandle),
    get_env_or_throw("BLUESKY_PASSWORD", BlueskyPassword, missing_credentials('Please export BLUESKY_PASSWORD environment variable.')).

% credentials_bluesky_handle(-BlueskyHandle).
credentials_bluesky_handle(BlueskyHandle) :-
    get_env_or_throw("BLUESKY_HANDLE", BlueskyHandle, missing_credentials('Please export BLUESKY_HANDLE environment variable.')).

% credentials_access_jwt(-AccessJwt).
credentials_access_jwt(AccessJwt) :-
    get_env_or_throw("ACCESS_JWT", AccessJwt, missing_access_jwt('Please export ACCESS_JWT environment variable.')).

% credentials_refresh_jwt(-RefreshJwt).
credentials_refresh_jwt(RefreshJwt) :-
    get_env_or_throw("REFRESH_JWT", RefreshJwt, missing_access_jwt('Please export REFRESH_JWT environment variable.')).

% temporary_dir(-TempDir).
temporary_dir(TempDir) :-
    get_env_or_throw("TEMP_DIR", TempDir, missing_temp_dir('Please export TEMP_DIR environment variable.')).
