:- module(configuration, [
    credentials/2,
    credentials_bluesky_handle/1,
    credentials_access_jwt/1,
    credentials_refresh_jwt/1,
    database_db_name/1,
    database_host/1,
    database_password/1,
    database_port/1,
    database_username/1,
    environment/1,
    log_level/1,
    list_at_uri/1,
    personal_data_server_host/1,
    public_bluesky_app_view_api_host/1,
    temporary_dir/1
]).
:- use_module(library(clpz)).
:- use_module(library(os)).
:- use_module(library(lists)).
:- use_module(library(serialization/json)).

/**
Environment-variable–backed configuration accessors.

Every predicate here resolves a single piece of runtime
configuration (Bluesky credentials, host names, database
connection parameters, log level, temp dir) from a fixed
environment variable, throwing a tagged error if the variable
is undeclared or empty. Used as a central seam by every
module that needs deployment-specific values.
*/

%% get_env_or_throw(+Name, -Value, +Error).
assert_env_var_is_declared(_Name, Value, _Error) :-
    getenv(_Name, Value).
assert_env_var_is_declared(Name, _Value, Error) :-
    \+ getenv(Name, _),
    throw(Error).

assert_env_var_non_empty(_Name, Value) :-
    length(Value, N),
    N #> 0.
assert_env_var_non_empty(Name, Value) :-
    \+ ( length(Value, N), N #> 0 ),
    throw(empty_env_var_value(Name)).

%% personal_data_server_host(-PersonalDataServerHost)
%
% PDS host name from the `PDSHOST` environment variable.
personal_data_server_host(PersonalDataServerHost) :-
    assert_env_var_is_declared("PDSHOST", PersonalDataServerHost, missing_personal_data_server_host('Please export PDSHOST environment variable.')),
    assert_env_var_non_empty("PDSHOST", PersonalDataServerHost).

%% public_bluesky_app_view_api_host(-PublicBlueskyAppViewApiHost)
%
% AppView API host from `PUBLIC_BLUESKY_APP_VIEW_API_HOST`.
public_bluesky_app_view_api_host(PublicBlueskyAppViewApiHost) :-
    assert_env_var_is_declared("PUBLIC_BLUESKY_APP_VIEW_API_HOST", PublicBlueskyAppViewApiHost, missing_public_bluesky_app_view_api_host('Please export PUBLIC_BLUESKY_APP_VIEW_API_HOST environment variable.')),
    assert_env_var_non_empty("PUBLIC_BLUESKY_APP_VIEW_API_HOST", PublicBlueskyAppViewApiHost).

%% credentials(-BlueskyHandle, -BlueskyPassword)
%
% Bluesky handle and password from `BLUESKY_HANDLE` and
% `BLUESKY_PASSWORD`. Used as the input to `createSession`
% when no session token is cached.
credentials(BlueskyHandle, BlueskyPassword) :-
    credentials_bluesky_handle(BlueskyHandle),
    assert_env_var_is_declared("BLUESKY_PASSWORD", BlueskyPassword, missing_credentials('Please export BLUESKY_PASSWORD environment variable.')),
    assert_env_var_non_empty("BLUESKY_PASSWORD", BlueskyPassword).

%% credentials_bluesky_handle(-BlueskyHandle)
%
% Bluesky handle from `BLUESKY_HANDLE`.
credentials_bluesky_handle(BlueskyHandle) :-
    assert_env_var_is_declared("BLUESKY_HANDLE", BlueskyHandle, missing_credentials('Please export BLUESKY_HANDLE environment variable.')),
    assert_env_var_non_empty("BLUESKY_HANDLE", BlueskyHandle).

%% credentials_access_jwt(-AccessJwt)
%
% Short-lived access JWT from `ACCESS_JWT`. Sent as a Bearer
% token on every authenticated XRPC request.
credentials_access_jwt(AccessJwt) :-
    assert_env_var_is_declared("ACCESS_JWT", AccessJwt, missing_access_jwt('Please export ACCESS_JWT environment variable.')),
    assert_env_var_non_empty("ACCESS_JWT", AccessJwt).

%% credentials_refresh_jwt(-RefreshJwt)
%
% Long-lived refresh JWT from `REFRESH_JWT`, exchanged for a
% new access JWT via `refreshSession`.
credentials_refresh_jwt(RefreshJwt) :-
    assert_env_var_is_declared("REFRESH_JWT", RefreshJwt, missing_access_jwt('Please export REFRESH_JWT environment variable.')),
    assert_env_var_non_empty("REFRESH_JWT", RefreshJwt).

%% environment(-Environment)
%
% Deployment environment name from `ENVIRONMENT`
% (e.g. `"development"`, `"testing"`, `"production"`). Drives
% feature switches such as the `.test` TLD allow-list in
% `handle.pl`.
environment(Environment) :-
    assert_env_var_is_declared("ENVIRONMENT", Environment, missing_environment('Please export ENVIRONMENT environment variable.')),
    assert_env_var_non_empty("ENVIRONMENT", Environment).

%% log_level(-LogLevel)
%
% Verbosity threshold from `LOG_LEVEL`. Consumed by
% `logger.pl` to gate debug/info/error output.
log_level(LogLevel) :-
    assert_env_var_is_declared("LOG_LEVEL", LogLevel, missing_log_level('Please export LOG_LEVEL environment variable.')),
    assert_env_var_non_empty("LOG_LEVEL", LogLevel).

%% temporary_dir(-TempDir)
%
% Scratch directory from `TEMP_DIR`. Used by `os_ext.pl` to
% name uuid-suffixed temporary files for `date(1)` shell-outs
% and JSON beautification.
temporary_dir(TempDir) :-
    assert_env_var_is_declared("TEMP_DIR", TempDir, missing_temp_dir('Please export TEMP_DIR environment variable.')),
    assert_env_var_non_empty("TEMP_DIR", TempDir).

%% list_at_uri(-ListAtUri)
%
% AT-URI of the curated list of authors to follow, from
% `LIST_AT_URI`. Drives the worker's main fan-out.
list_at_uri(ListAtUri) :-
    assert_env_var_is_declared("LIST_AT_URI", ListAtUri, missing_temp_dir('Please export LIST_AT_URI environment variable.')),
    assert_env_var_non_empty("LIST_AT_URI", ListAtUri).

%% database_host(-DatabaseHost)
%
% Postgres host name from `DATABASE_HOST`.
database_host(DatabaseHost) :-
    assert_env_var_is_declared("DATABASE_HOST", DatabaseHost, missing_database_parameter('Please export DATABASE_HOST environment variable.')),
    assert_env_var_non_empty("DATABASE_HOST", DatabaseHost).

%% database_password(-DatabasePassword)
%
% Postgres password from `DATABASE_PASSWORD`. Sent to the
% server only through the SCRAM-SHA-256 handshake — never in
% the clear.
database_password(DatabasePassword) :-
    assert_env_var_is_declared("DATABASE_PASSWORD", DatabasePassword, missing_database_parameter('Please export DATABASE_PASSWORD environment variable.')),
    assert_env_var_non_empty("DATABASE_PASSWORD", DatabasePassword).

%% database_port(-DatabasePort)
%
% Postgres TCP port from `DATABASE_PORT`.
database_port(DatabasePort) :-
    assert_env_var_is_declared("DATABASE_PORT", DatabasePort, missing_database_parameter('Please export DATABASE_PORT environment variable.')),
    assert_env_var_non_empty("DATABASE_PORT", DatabasePort).

%% database_username(-DatabaseUsername)
%
% Postgres role from `DATABASE_USERNAME`.
database_username(DatabaseUsername) :-
    assert_env_var_is_declared("DATABASE_USERNAME", DatabaseUsername, missing_database_parameter('Please export DATABASE_USERNAME environment variable.')),
    assert_env_var_non_empty("DATABASE_USERNAME", DatabaseUsername).

%% database_db_name(-DatabaseDbName)
%
% Postgres database name from `DATABASE_DB_NAME`.
database_db_name(DatabaseDbName) :-
    assert_env_var_is_declared("DATABASE_DB_NAME", DatabaseDbName, missing_database_parameter('Please export DATABASE_DB_NAME environment variable.')),
    assert_env_var_non_empty("DATABASE_DB_NAME", DatabaseDbName).