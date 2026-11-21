:- module(settings, [
    setting/3,
    setting_variable/2,
    setting_variables/1
]).

:- use_module(library(lists)).
:- use_module(library(reif)).

/**
Relates an environment, taken as data, to every setting the worker runs on.

`configuration.pl` used to ask `getenv/2` twenty times over and to spell
each default as `\+ getenv(Name, _)`. A default stated that way cannot be
proved: negation as failure makes the clause true of whatever cannot be
derived, so nothing distinguishes "the deployment left this out, and five
seconds is what we meant" from "we never thought about it". The defaults
are the part of the configuration most worth proving — a wrong one is a
wrong deployment, and a silent one.

So the environment arrives here as a list of `Name-Value` pairs and
nothing in this module looks at the process it came from. Presence is a
reified question (`memberd_t/3` over the names), resolution hands back a
tagged term the caller dispatches on, and all of it is definite clauses,
so `settings/1` reads as a table one can check line by line against the
deployment notes.

Three outcomes, because what a missing value means is the caller's
business — only the caller knows how to stop a run:

  * `setting(V)` — what the environment says, or the documented default;
  * `empty` — declared with nothing in it, where a value is demanded;
  * `undeclared` — absent, and no default to fall back on.

Values are handed over as the environment spells them, chars, except
where a setting is a count: a pace and a batch size are numbers at every
call site, so they are read as numbers once, here.
*/

%% settings(-Specs)
%
% One `setting(Name, Variable, Expectation)` per line: the whole
% configuration surface in one place. `Expectation` says what an absent
% variable means.
%
%   * `required` — stop. No default would be right, since starting
%     against the wrong host, or with no password, is worse for a
%     deployment than refusing to start at all.
%   * `default(Chars)` — the documented fallback, as chars.
%   * `counted(Number)` — the documented fallback, and the value is read
%     as a number whether it comes from the environment or from here.
%
% A declared variable is taken as it stands under `default` and
% `counted`: an empty `PG_BACKEND=` is an empty backend name, not "wire",
% and every dispatch clause then fails at its guard — a Prolog failure at
% the call site is easier to root-cause than a swallowed default.
settings([
    setting(credentials_bluesky_handle, "BLUESKY_HANDLE", required),
    setting(credentials_bluesky_password, "BLUESKY_PASSWORD", required),
    setting(credentials_access_jwt, "ACCESS_JWT", required),
    setting(credentials_refresh_jwt, "REFRESH_JWT", required),

    setting(database_host, "DATABASE_HOST", required),
    setting(database_port, "DATABASE_PORT", required),
    setting(database_db_name, "DATABASE_DB_NAME", required),
    setting(database_username, "DATABASE_USERNAME", required),
    setting(database_password, "DATABASE_PASSWORD", required),

    setting(environment, "ENVIRONMENT", required),
    setting(log_level, "LOG_LEVEL", required),
    setting(list_at_uri, "LIST_AT_URI", required),
    setting(personal_data_server_host, "PDSHOST", required),
    setting(public_bluesky_app_view_api_host, "PUBLIC_BLUESKY_APP_VIEW_API_HOST", required),
    setting(temporary_dir, "TEMP_DIR", required),

    % The wire transport is the default so that deployments already on it
    % stay there without exporting anything; "psql" shells out instead.
    setting(pg_backend, "PG_BACKEND", default("wire")),

    % Unset means links are resolved. The switch exists so a redirector
    % that starts hanging, or a ledger that needs taking down, can be
    % answered by restarting the worker with one variable rather than by
    % deploying.
    setting(link_expansion, "LINK_EXPANSION", default("on")),

    % A name and an address to complain to: the redirectors French
    % publishers share answer differently depending on who asks, and
    % being able to say who we are without a deploy is what lets a run be
    % corrected rather than redeployed.
    setting(link_expander_user_agent,
            "LINK_EXPANDER_USER_AGENT",
            default("revue-de-presse-link-expander/1.0 (+https://revue-de-presse.org)")),

    % Five seconds between two requests: a reader opening articles, not a
    % crawler. A run with no pace at all asked eight
    % hundred times in a row and was shut out.
    setting(link_expander_pace, "LINK_EXPANDER_PACE", counted(5)),

    % Sixty links at five seconds apart is five minutes of asking, which
    % is what a day of new links needs and no more.
    setting(link_expansion_batch, "LINK_EXPANSION_BATCH", counted(60))
]).

%% setting(+Setting, +Env, -Resolved)
%
% Relates a setting and an environment to `setting(V)`, `empty` or
% `undeclared`. A name that is in no spec has no answer, the same way an
% unexported accessor has none.
setting(Setting, Env, Resolved) :-
    settings(Specs),
    spec(Setting, Specs, found(Variable-Expectation)),
    env_lookup(Variable, Env, Found),
    expected_resolved(Expectation, Found, Resolved).

%% setting_variable(+Setting, -Variable)
%
% Which variable a setting is read from, so a caller reporting on one can
% name the variable a deployer has to export.
setting_variable(Setting, Variable) :-
    settings(Specs),
    spec(Setting, Specs, found(Variable-_)).

%% setting_variables(-Variables)
%
% Every variable the table names, for a caller that would rather read the
% environment in one pass than one variable at a time.
setting_variables(Variables) :-
    settings(Specs),
    spec_variables(Specs, Variables).

spec_variables([], []).
spec_variables([setting(_, Variable, _)|Specs], [Variable|Variables]) :-
    spec_variables(Specs, Variables).

%% spec(+Setting, +Specs, -Found)
%
% `found(Variable-Expectation)` or `none`: a search that hands back a tag
% rather than failing, so a caller can tell a misspelt setting name from
% a variable nobody exported.
spec(_, [], none).
spec(Setting, [setting(Name, Variable, Expectation)|Specs], Found) :-
    if_(Setting = Name,
        Found = found(Variable-Expectation),
        spec(Setting, Specs, Found)).

%% expected_resolved(+Expectation, +Found, -Resolved)
%
% What the environment's answer amounts to, given what the setting
% expects. The two `undeclared` cases are the defaults: here, as facts,
% rather than as the shadow of a goal that could not be proved.
expected_resolved(required, undeclared, undeclared).
expected_resolved(required, declared(Chars), Resolved) :-
    demanded_resolved(Chars, Resolved).
expected_resolved(default(Default), undeclared, setting(Default)).
expected_resolved(default(_), declared(Chars), setting(Chars)).
expected_resolved(counted(Default), undeclared, setting(Default)).
expected_resolved(counted(_), declared(Chars), setting(Number)) :-
    number_chars(Number, Chars).

%% demanded_resolved(+Chars, -Resolved)
%
% An exported but empty variable is its own kind of wrong: the deployer
% meant to say something and said nothing, which is worth a different
% word from never having exported it.
demanded_resolved(Chars, Resolved) :-
    length(Chars, Length),
    if_(Length = 0, Resolved = empty, Resolved = setting(Chars)).

%% env_lookup(+Variable, +Env, -Found)
%
% `declared(Chars)` for the first pair under that name, `undeclared`
% otherwise. Presence is asked of the names alone: `memberd_t/3` on a
% whole `Name-Value` pair, with the value still free, would answer twice
% — once unifying the value, once refusing it — and a lookup that guesses
% is no lookup.
env_lookup(Variable, Env, Found) :-
    env_names(Env, Names),
    if_(memberd_t(Variable, Names),
        first_value(Variable, Env, Found),
        Found = undeclared).

env_names([], []).
env_names([Name-_|Pairs], [Name|Names]) :-
    env_names(Pairs, Names).

% Reached only once the names are known to carry Variable, so the walk
% ends on a pair and never on the empty list.
first_value(Variable, [Name-Value|Pairs], Found) :-
    if_(Variable = Name,
        Found = declared(Value),
        first_value(Variable, Pairs, Found)).
