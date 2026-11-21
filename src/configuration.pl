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
    link_expander_pace/1,
    link_expander_user_agent/1,
    link_expansion/1,
    link_expansion_batch/1,
    list_at_uri/1,
    personal_data_server_host/1,
    pg_backend/1,
    public_bluesky_app_view_api_host/1,
    temporary_dir/1
]).
:- use_module(library(os)).

:- use_module(settings, [
    setting/3,
    setting_variable/2
]).

/**
Environment-variable–backed configuration accessors.

Every predicate here resolves a single piece of runtime configuration
(Bluesky credentials, host names, database connection parameters, log
level, temp dir) from a fixed environment variable, throwing a tagged
error if the variable is undeclared or empty. Used as a central seam by
every module that needs deployment-specific values.

What a setting is — which variable it is read from, what an absent one
falls back to — is not decided here but in `settings.pl`, which takes the
environment as a list of `Name-Value` pairs and relates it to every
setting in definite clauses. This module is only the one place where that
list is obtained from the running process, which is what makes the rest
provable: `declared/2` below holds this module's single negation, and
there is no way round it, since a variable nobody exported makes
`getenv/2` fail rather than raise, and failure can only be read with
`\+`.

The exported predicates and the errors they throw are unchanged, so no
caller needs to know any of this happened.
*/

%% resolved(+Setting, -Value)
%
% Reads the variable this setting is named after, hands it to the pure
% module as the whole environment it should consider, and either yields
% the value or stops.
%
% Only that one variable is read, although `settings:setting_variables/1`
% would offer them all: `pg_backend/1` guards every repository dispatch
% and `log_level/1` runs on every log line, and neither has any business
% asking the process about twenty variables it will not look at.
resolved(Setting, Value) :-
    setting_variable(Setting, Variable),
    declared(Variable, Environment),
    setting(Setting, Environment, Resolved),
    resolved_value(Setting, Resolved, Value).

%% declared(+Variable, -Environment)
%
% `[Variable-Value]` when the process declares it, `[]` otherwise.
declared(Variable, [Variable-Value]) :-
    getenv(Variable, Value).
declared(Variable, []) :-
    \+ getenv(Variable, _).

%% resolved_value(+Setting, +Resolved, -Value)
%
% Turns what the pure module found into what callers have always seen: a
% value, or the tagged error the deployer is meant to read.
resolved_value(_Setting, setting(Value), Value).
resolved_value(Setting, undeclared, _Value) :-
    undeclared_error(Setting, Error),
    throw(Error).
resolved_value(Setting, empty, _Value) :-
    setting_variable(Setting, Variable),
    throw(empty_env_var_value(Variable)).

%% undeclared_error(?Setting, ?Error)
%
% What is thrown when a required variable is missing. The message names
% the variable to export, since that is the only thing the reader of a
% crashed run can act on. Several settings share a functor on purpose —
% the five database parameters are one failure to a deployer — and two
% keep a functor that reads oddly (`REFRESH_JWT` reports as
% `missing_access_jwt`, `LIST_AT_URI` as `missing_temp_dir`); they are
% left as they were, because something may well be matching on them.
undeclared_error(credentials_bluesky_handle,
                 missing_credentials('Please export BLUESKY_HANDLE environment variable.')).
undeclared_error(credentials_bluesky_password,
                 missing_credentials('Please export BLUESKY_PASSWORD environment variable.')).
undeclared_error(credentials_access_jwt,
                 missing_access_jwt('Please export ACCESS_JWT environment variable.')).
undeclared_error(credentials_refresh_jwt,
                 missing_access_jwt('Please export REFRESH_JWT environment variable.')).
undeclared_error(database_host,
                 missing_database_parameter('Please export DATABASE_HOST environment variable.')).
undeclared_error(database_port,
                 missing_database_parameter('Please export DATABASE_PORT environment variable.')).
undeclared_error(database_db_name,
                 missing_database_parameter('Please export DATABASE_DB_NAME environment variable.')).
undeclared_error(database_username,
                 missing_database_parameter('Please export DATABASE_USERNAME environment variable.')).
undeclared_error(database_password,
                 missing_database_parameter('Please export DATABASE_PASSWORD environment variable.')).
undeclared_error(environment,
                 missing_environment('Please export ENVIRONMENT environment variable.')).
undeclared_error(log_level,
                 missing_log_level('Please export LOG_LEVEL environment variable.')).
undeclared_error(list_at_uri,
                 missing_temp_dir('Please export LIST_AT_URI environment variable.')).
undeclared_error(personal_data_server_host,
                 missing_personal_data_server_host('Please export PDSHOST environment variable.')).
undeclared_error(public_bluesky_app_view_api_host,
                 missing_public_bluesky_app_view_api_host('Please export PUBLIC_BLUESKY_APP_VIEW_API_HOST environment variable.')).
undeclared_error(temporary_dir,
                 missing_temp_dir('Please export TEMP_DIR environment variable.')).

%% personal_data_server_host(-PersonalDataServerHost)
%
% PDS host name from the `PDSHOST` environment variable.
personal_data_server_host(PersonalDataServerHost) :-
    resolved(personal_data_server_host, PersonalDataServerHost).

%% public_bluesky_app_view_api_host(-PublicBlueskyAppViewApiHost)
%
% AppView API host from `PUBLIC_BLUESKY_APP_VIEW_API_HOST`.
public_bluesky_app_view_api_host(PublicBlueskyAppViewApiHost) :-
    resolved(public_bluesky_app_view_api_host, PublicBlueskyAppViewApiHost).

%% credentials(-BlueskyHandle, -BlueskyPassword)
%
% Bluesky handle and password from `BLUESKY_HANDLE` and
% `BLUESKY_PASSWORD`. Used as the input to `createSession`
% when no session token is cached.
credentials(BlueskyHandle, BlueskyPassword) :-
    credentials_bluesky_handle(BlueskyHandle),
    resolved(credentials_bluesky_password, BlueskyPassword).

%% credentials_bluesky_handle(-BlueskyHandle)
%
% Bluesky handle from `BLUESKY_HANDLE`.
credentials_bluesky_handle(BlueskyHandle) :-
    resolved(credentials_bluesky_handle, BlueskyHandle).

%% credentials_access_jwt(-AccessJwt)
%
% Short-lived access JWT from `ACCESS_JWT`. Sent as a Bearer
% token on every authenticated XRPC request.
credentials_access_jwt(AccessJwt) :-
    resolved(credentials_access_jwt, AccessJwt).

%% credentials_refresh_jwt(-RefreshJwt)
%
% Long-lived refresh JWT from `REFRESH_JWT`, exchanged for a
% new access JWT via `refreshSession`.
credentials_refresh_jwt(RefreshJwt) :-
    resolved(credentials_refresh_jwt, RefreshJwt).

%% environment(-Environment)
%
% Deployment environment name from `ENVIRONMENT`
% (e.g. `"development"`, `"testing"`, `"production"`). Drives
% feature switches such as the `.test` TLD allow-list in
% `handle.pl`.
environment(Environment) :-
    resolved(environment, Environment).

%% log_level(-LogLevel)
%
% Verbosity threshold from `LOG_LEVEL`. Consumed by
% `logger.pl` to gate debug/info/error output.
log_level(LogLevel) :-
    resolved(log_level, LogLevel).

%% temporary_dir(-TempDir)
%
% Scratch directory from `TEMP_DIR`. Used by `os_ext.pl` to
% name uuid-suffixed temporary files for `date(1)` shell-outs
% and JSON beautification.
temporary_dir(TempDir) :-
    resolved(temporary_dir, TempDir).

%% list_at_uri(-ListAtUri)
%
% AT-URI of the curated list of authors to follow, from
% `LIST_AT_URI`. Drives the worker's main fan-out.
list_at_uri(ListAtUri) :-
    resolved(list_at_uri, ListAtUri).

%% database_host(-DatabaseHost)
%
% Postgres host name from `DATABASE_HOST`.
database_host(DatabaseHost) :-
    resolved(database_host, DatabaseHost).

%% database_password(-DatabasePassword)
%
% Postgres password from `DATABASE_PASSWORD`. Sent to the
% server only through the SCRAM-SHA-256 handshake — never in
% the clear.
database_password(DatabasePassword) :-
    resolved(database_password, DatabasePassword).

%% database_port(-DatabasePort)
%
% Postgres TCP port from `DATABASE_PORT`.
database_port(DatabasePort) :-
    resolved(database_port, DatabasePort).

%% database_username(-DatabaseUsername)
%
% Postgres role from `DATABASE_USERNAME`.
database_username(DatabaseUsername) :-
    resolved(database_username, DatabaseUsername).

%% database_db_name(-DatabaseDbName)
%
% Postgres database name from `DATABASE_DB_NAME`.
database_db_name(DatabaseDbName) :-
    resolved(database_db_name, DatabaseDbName).

%% link_expansion(-Setting)
%
% Whether ingestion resolves the shortened links a post carries, from
% `LINK_EXPANSION`. Two values:
%
%   - "on"  — resolve and store the article's address (default);
%   - "off" — store the post exactly as written.
link_expansion(Setting) :-
    resolved(link_expansion, Setting).

%% link_expander_user_agent(-UserAgent)
%
% What the link expander calls itself, from `LINK_EXPANDER_USER_AGENT`.
%
% It is a setting and not a constant because the redirectors most French
% publishers share answer differently depending on who asks: a plain client
% is served an interstitial carrying the destination, and the same client was
% refused outright once it had asked too often.
%
% Whatever it says, it says it honestly: a name and an address to complain to.
link_expander_user_agent(UserAgent) :-
    resolved(link_expander_user_agent, UserAgent).

%% link_expander_pace(-Seconds)
%
% Seconds between two requests in a batch run, from `LINK_EXPANDER_PACE`,
% as a number.
link_expander_pace(Seconds) :-
    resolved(link_expander_pace, Seconds).

%% link_expansion_batch(-Links)
%
% How many links one batch run may ask about, from `LINK_EXPANSION_BATCH`,
% as a number.
link_expansion_batch(Links) :-
    resolved(link_expansion_batch, Links).

%% pg_backend(-Backend)
%
% Active Postgres transport, from `PG_BACKEND`. Two values:
%
%   - "wire" — postgresql-prolog over TCP (default; SCRAM-SHA-256).
%   - "psql" — `psql(1)` shell-out + `PGPASSWORD` env (fall-back).
%
% `pg_backend("wire")` and `pg_backend("psql")` are mutually exclusive at
% runtime, so each is used as a guard goal at the head of dispatching
% repository predicates.
pg_backend(Backend) :-
    resolved(pg_backend, Backend).
