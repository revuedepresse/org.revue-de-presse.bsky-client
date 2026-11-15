:- module(repository_publication_link, [
    attempts_since/3,
    expansion_of/3,
    record_pending/3,
    record_expansion/4,
    record_refusal/4
]).

:- use_module(library(dif)).
:- use_module(library(lists)).

:- use_module('../pg/client', [
    execute/3,
    pg_query_or_throw/4
]).
:- use_module('../../configuration', [pg_backend/1]).
:- use_module('../../stream', [writeln/2]).

/**
Repository for `publication_link`, the ledger of shortened links.

A row is keyed by the shortened address itself, so a token shared by a
hundred posts is asked about once. Reads answer one of three things:

  * `present(expanded(Url))` - the address to store instead;
  * `present(Status)` - known, and not to be asked about again
    (`pending`, `refused`, `dead`);
  * `absent` - never seen.

Wire-path predicates take the usual `pg_session(In, Out)`. On the psql
fall-back the ledger is skipped entirely: it is an optimisation, and
without it a link is simply left as the post wrote it.
*/

table("publication_link").

%% expansion_of(+Session, +short_url(Url), -Found)
expansion_of(pg_session(C0, C), short_url(Url), Found) :-
    pg_backend("wire"),
    lookup_sql(SQL),
    pg_query_or_throw(pg_session(C0, C), SQL, [Url], Reply),
    interpret_lookup(Reply, Found).
expansion_of(pg_session(C, C), short_url(_), absent) :-
    pg_backend("psql").

lookup_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT status, coalesce(expanded_url, '') ",
            "FROM public.", Table, " ",
            "WHERE short_url = $1 ",
            "LIMIT 1"
        ],
        SQL
    ).

interpret_lookup(data([]), absent).
interpret_lookup(data([[Status, Expanded|_]|_]), Found) :-
    interpret_row(Status, Expanded, Found).
interpret_lookup(error(Err), _) :-
    throw(pg_error(Err)).

interpret_row(Status, Expanded, present(expanded(Expanded))) :-
    Status == "resolved",
    \+ Expanded == "",
    \+ Expanded == null.
interpret_row(Status, Expanded, present(Status)) :-
    (   \+ Status == "resolved"
    ;   Expanded == ""
    ;   Expanded == null
    ),
    !.

%% record_pending(+Session, +short_url(Url), -Outcome)
%
% Notes a link whose destination nobody knows yet. Re-seeing it only
% bumps the counter: the row keeps whatever the resolver has written.
record_pending(pg_session(C0, C), short_url(Url), ok) :-
    pg_backend("wire"),
    pending_sql(SQL),
    execute(pg_session(C0, C), SQL, [Url]),
    writeln([link_pending|[Url]], true).
record_pending(pg_session(C, C), short_url(_), skipped) :-
    pg_backend("psql").

pending_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " ",
            "(short_url, status, attempts, first_seen_at, last_attempt_at) ",
            "VALUES ($1, 'pending', 0, now()::text, NULL) ",
            "ON CONFLICT (short_url) DO NOTHING"
        ],
        SQL
    ).

%% record_expansion(+Session, +short_url(Url), +expanded(Expanded), -Outcome)
record_expansion(pg_session(C0, C), short_url(Url), expanded(Expanded), ok) :-
    pg_backend("wire"),
    expansion_sql(SQL),
    execute(pg_session(C0, C), SQL, [Url, Expanded]),
    writeln([link_resolved|[Url]], true).
record_expansion(pg_session(C, C), short_url(_), expanded(_), skipped) :-
    pg_backend("psql").

expansion_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " ",
            "(short_url, expanded_url, status, http_status, attempts, first_seen_at, last_attempt_at) ",
            "VALUES ($1, $2, 'resolved', 200, 1, now()::text, now()::text) ",
            "ON CONFLICT (short_url) DO UPDATE SET ",
            "expanded_url = EXCLUDED.expanded_url, ",
            "status = 'resolved', ",
            "http_status = EXCLUDED.http_status, ",
            "attempts = public.", Table, ".attempts + 1, ",
            "last_attempt_at = EXCLUDED.last_attempt_at"
        ],
        SQL
    ).

%% record_refusal(+Session, +short_url(Url), +http_status(Code), -Outcome)
%
% The service answered, and not with a destination. Stored so the link is
% never asked about again on the ingest path.
record_refusal(pg_session(C0, C), short_url(Url), http_status(Code), ok) :-
    pg_backend("wire"),
    refusal_status(Code, Status),
    number_chars(Code, CodeChars),
    refusal_sql(SQL),
    execute(pg_session(C0, C), SQL, [Url, Status, CodeChars]),
    writeln([link_refused|[Url, Code]], true).
record_refusal(pg_session(C, C), short_url(_), http_status(_), skipped) :-
    pg_backend("psql").

% Only the two answers that are about the link itself are final. A 403 or a
% 429 is about the asker — this address, this rate, this client — and on
% 2026-11-15 one run's burst turned 812 live links into permanent refusals
% that way. Those are `blocked`: left for a resolver asking from somewhere
% else, and never counted as a verdict on the link.
refusal_status(404, "dead").
refusal_status(410, "dead").
refusal_status(Code, "blocked") :-
    dif(Code, 404),
    dif(Code, 410).

refusal_sql(SQL) :-
    table(Table),
    append(
        [
            "INSERT INTO public.", Table, " ",
            "(short_url, status, http_status, attempts, first_seen_at, last_attempt_at) ",
            "VALUES ($1, $2, $3::integer, 1, now()::text, now()::text) ",
            "ON CONFLICT (short_url) DO UPDATE SET ",
            "status = EXCLUDED.status, ",
            "http_status = EXCLUDED.http_status, ",
            "attempts = public.", Table, ".attempts + 1, ",
            "last_attempt_at = EXCLUDED.last_attempt_at"
        ],
        SQL
    ).

%% attempts_since(+Session, +minutes(Minutes), -Count)
%
% How many links have been asked about in the last Minutes. The ledger is
% the rate limiter: it is shared by every run and survives restarts, which a
% counter inside one run is not. Ingestion asks it before reaching for a
% service it has never asked about.
attempts_since(pg_session(C0, C), minutes(Minutes), Count) :-
    pg_backend("wire"),
    number_chars(Minutes, MinuteChars),
    attempts_sql(SQL),
    pg_query_or_throw(pg_session(C0, C), SQL, [MinuteChars], Reply),
    interpret_count(Reply, Count).
attempts_since(pg_session(C, C), minutes(_), 0) :-
    pg_backend("psql").

attempts_sql(SQL) :-
    table(Table),
    append(
        [
            "SELECT count(*)::text ",
            "FROM public.", Table, " ",
            "WHERE last_attempt_at IS NOT NULL ",
            "AND last_attempt_at::timestamptz > now() - ($1 || ' minutes')::interval"
        ],
        SQL
    ).

interpret_count(data([[Chars|_]|_]), Count) :-
    number_chars(Count, Chars).
interpret_count(data([]), 0).
interpret_count(error(Err), _) :-
    throw(pg_error(Err)).
