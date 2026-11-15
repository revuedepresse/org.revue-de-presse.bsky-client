:- module(expand_day_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [
    close_pg_connection/1,
    open_pg_connection/1,
    pg_query/4,
    pg_query_simple/3
]).
:- use_module('../../src/link_shortener', [shortened_url_t/2]).
:- use_module('../../src/infrastructure/repository/repository_publication_link', [
    links_of_day/3,
    record_expansion/4,
    rewrite_links_of_day/4
]).

/*
The day pass, without the network: what it reads out of a day's rows, and
what it writes back into them.

  1. a day's links that the ledger cannot answer are listed from the rows
     themselves, decoded from the base64 they are stored as;
  2. a link the ledger has resolved is put into the publication and the
     status row, in place of the token;
  3. a link already resolved is not listed again, so a second run asks
     nobody about it.

Run with:
  make test-expand-day
*/

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

day("2025-07-04").
short("https://l.probe-expand-day.test/Yy7").
expanded("https://www.mediapart.fr/journal/france/040725/enquete").
uri("at://probe-expand-day-0").

expect(Label, Got, Expected) :-
    (   Got == Expected
    ->  format("[ok] ~s~n", [Label])
    ;   format("[KO] ~s~n  expected ~q~n  got      ~q~n", [Label, Expected, Got]), halt(1) ).

cleanup(Conn) :-
    pg_query_simple(Conn, "DELETE FROM publication      WHERE document_id   LIKE 'at://probe-expand-day-%'", _),
    pg_query_simple(Conn, "DELETE FROM weaving_status   WHERE ust_status_id LIKE 'at://probe-expand-day-%'", _),
    pg_query_simple(Conn, "DELETE FROM publication_link WHERE short_url     LIKE 'https://l.probe-expand-day%'", _).

%% seed(+Conn, +Day, +Short)
%
% A stored post, text base64-encoded as the repository writes it.
seed(Conn, Day, Short) :-
    uri(URI),
    append(["INSERT INTO publication (id, legacy_id, hash, screen_name, text, avatar_url, document_id, document, published_at) VALUES ('probe-expand-day', 1, 'probe-expand-day-hash', 'probe.bsky.social', encode(convert_to('\"Notre enquête 👉 ", Short, "\"', 'UTF8'), 'base64'), '', '", URI, "', '{}', '", Day, " 09:00:00')"], PublicationSQL),
    pg_query_simple(Conn, PublicationSQL, _),
    append(["INSERT INTO weaving_status (ust_hash, ust_name, ust_full_name, ust_text, ust_avatar, ust_api_document, ust_status_id, ust_access_token, is_published, ust_created_at) VALUES ('probe-expand-day-hash', 'probe.bsky.social', 'probe.bsky.social', encode(convert_to('\"Notre enquête 👉 ", Short, "\"', 'UTF8'), 'base64'), '', '{}', '", URI, "', 'dummy', 'true', '", Day, " 09:00:00')"], StatusSQL),
    pg_query_simple(Conn, StatusSQL, _).

decoded(Conn, Table, Column, Key, Text) :-
    append(["SELECT convert_from(decode(", Column, ", 'base64'), 'UTF8') FROM ", Table, " WHERE ", Key], SQL),
    pg_query(Conn, SQL, [], data([[Text|_]|_])).

contains(Needle, Haystack) :-
    append(_, Suffix, Haystack),
    append(Needle, _, Suffix),
    !.

run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    day(Day),
    short(Short),
    expanded(Expanded),

    open_pg_connection(C0),
    cleanup(C0),
    seed(C0, Day, Short),

    % 1. the day's links, read back out of the rows
    links_of_day(pg_session(C0, C1), day(Day), Listed),
    expect("the day's unanswered links are read from the rows themselves",
           Listed, [Short]),

    % 2. what the ledger knows is written into the day's rows
    record_expansion(pg_session(C1, C2), short_url(Short), expanded(Expanded), _),
    rewrite_links_of_day(pg_session(C2, C3), day(Day), short_url(Short)-expanded(Expanded), Rewritten),
    expect("one publication row carried that link", Rewritten, 1),

    decoded(C3, "publication", "text", "document_id = 'at://probe-expand-day-0'", Publication),
    decoded(C3, "weaving_status", "ust_text", "ust_status_id = 'at://probe-expand-day-0'", Status),
    (   contains(Expanded, Publication),
        \+ contains(Short, Publication)
    ->  format("[ok] the publication row carries the address, not the token~n", [])
    ;   format("[KO] publication row reads ~s~n", [Publication]), halt(1) ),
    (   contains(Expanded, Status)
    ->  format("[ok] the status row says the same thing~n", [])
    ;   format("[KO] status row reads ~s~n", [Status]), halt(1) ),

    % 3. a resolved link is not asked about again. The listing is of every
    % address the day carries, so the expanded one appears — what matters is
    % that nothing shortened is left for the next run to ask about.
    links_of_day(pg_session(C3, C4), day(Day), Remaining),
    tfilter(shortened_url_t, Remaining, StillShortened),
    expect("nothing shortened is left for the next run", StillShortened, []),
    expect("and the day now carries the address instead", Remaining, [Expanded]),

    cleanup(C4),
    close_pg_connection(C4),
    format("[OK] expand_day_test passed~n", []),
    halt(0).
