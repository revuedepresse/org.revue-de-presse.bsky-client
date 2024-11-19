:- module(configuration, [
    credentials/2,
    credentials_bluesky_handle/1,
    credentials_access_jwt/1,
    credentials_refresh_jwt/1,
    environment/1,
    log_level/1,
    personal_data_server_host/1,
    public_bluesky_app_view_api_host/1,
    temporary_dir/1
]).
:- use_module(library(clpz)).
:- use_module(library(os)).
:- use_module(library(lists)).
:- use_module(library(serialization/json)).

% get_env_or_throw(+Name, -Value, +Error).
assert_env_var_is_declared(Name, Value, Error) :-
    (   getenv(Name, Value)
    ->  true
    ;   throw(Error)    ).

assert_env_var_non_empty(Name, Value) :-
    (   ( length(Value, N),
        N #> 0 )
    ->  true
    ;   throw(empty_env_var_value(Name))    ).

% personal_data_server_host(-PersonalDataServerHost).
personal_data_server_host(PersonalDataServerHost) :-
    assert_env_var_is_declared("PDSHOST", PersonalDataServerHost, missing_personal_data_server_host('Please export PDSHOST environment variable.')),
    assert_env_var_non_empty("PDSHOST", PersonalDataServerHost).

% public_bluesky_app_view_api_host(-PublicBlueskyAppViewApiHost).
public_bluesky_app_view_api_host(PublicBlueskyAppViewApiHost) :-
    assert_env_var_is_declared("PUBLIC_BLUESKY_APP_VIEW_API_HOST", PublicBlueskyAppViewApiHost, missing_public_bluesky_app_view_api_host('Please export PUBLIC_BLUESKY_APP_VIEW_API_HOST environment variable.')),
    assert_env_var_non_empty("PUBLIC_BLUESKY_APP_VIEW_API_HOST", PublicBlueskyAppViewApiHost).

% credentials(-BlueskyHandle, -BlueskyPassword).
credentials(BlueskyHandle, BlueskyPassword) :-
    credentials_bluesky_handle(BlueskyHandle),
    assert_env_var_is_declared("BLUESKY_PASSWORD", BlueskyPassword, missing_credentials('Please export BLUESKY_PASSWORD environment variable.')),
    assert_env_var_non_empty("BLUESKY_PASSWORD", BlueskyPassword).

% credentials_bluesky_handle(-BlueskyHandle).
credentials_bluesky_handle(BlueskyHandle) :-
    assert_env_var_is_declared("BLUESKY_HANDLE", BlueskyHandle, missing_credentials('Please export BLUESKY_HANDLE environment variable.')),
    assert_env_var_non_empty("BLUESKY_HANDLE", BlueskyHandle).

% credentials_access_jwt(-AccessJwt).
credentials_access_jwt(AccessJwt) :-
    assert_env_var_is_declared("ACCESS_JWT", AccessJwt, missing_access_jwt('Please export ACCESS_JWT environment variable.')),
    assert_env_var_non_empty("ACCESS_JWT", AccessJwt).

% credentials_refresh_jwt(-RefreshJwt).
credentials_refresh_jwt(RefreshJwt) :-
    assert_env_var_is_declared("REFRESH_JWT", RefreshJwt, missing_access_jwt('Please export REFRESH_JWT environment variable.')),
    assert_env_var_non_empty("REFRESH_JWT", RefreshJwt).

% environment(-Environment).
environment(Environment) :-
    assert_env_var_is_declared("ENVIRONMENT", Environment, missing_environment('Please export ENVIRONMENT environment variable.')),
    assert_env_var_non_empty("ENVIRONMENT", Environment).

% log_level(-LogLevel).
log_level(LogLevel) :-
    assert_env_var_is_declared("LOG_LEVEL", LogLevel, missing_log_level('Please export LOG_LEVEL environment variable.')),
    assert_env_var_non_empty("LOG_LEVEL", LogLevel).

% temporary_dir(-TempDir).
temporary_dir(TempDir) :-
    assert_env_var_is_declared("TEMP_DIR", TempDir, missing_temp_dir('Please export TEMP_DIR environment variable.')),
    assert_env_var_non_empty("TEMP_DIR", TempDir).
