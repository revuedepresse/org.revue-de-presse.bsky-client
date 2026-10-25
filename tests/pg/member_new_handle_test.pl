:- module(member_new_handle_test, [run_test/0]).

:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).

:- use_module('../../src/infrastructure/pg/connection', [
    close_pg_connection/1,
    open_pg_connection/1,
    pg_query_simple/3
]).
:- use_module('../../src/infrastructure/repository/repository_list_item', [
    insert_list_items_if_not_exists/3
]).

/*
A list member recorded in weaving_user under a handle it no longer
uses is found by its DID (usr_twitter_id), and its row moves to the
current handle: usr_twitter_username, plus usr_user_name,
usr_username_canonical and url, which insert_record/3 fills with the
handle. No second row is inserted for the DID, which a unique index
on usr_twitter_id forbids. Importing the member again under the same
handle leaves that single row as it is.

Run under both backends (PG_BACKEND unset or psql).
*/

env_or_die(Name) :-
    getenv(Name, _).
env_or_die(Name) :-
    \+ getenv(Name, _),
    format("[KO] missing env var ~s~n", [Name]),
    halt(2).

%% expect_count(+Conn, +SQL, +Expected, +Label)
expect_count(Conn, SQL, Expected, Label) :-
    pg_query_simple(Conn, SQL, Reply),
    report_count(Reply, Expected, Label).

report_count(data(_, [[Expected]]), Expected, Label) :-
    format("[ok] ~s: ~s~n", [Label, Expected]).
report_count(Reply, Expected, Label) :-
    \+ Reply = data(_, [[Expected]]),
    format("[KO] ~s: expected ~s, got ~q~n", [Label, Expected, Reply]),
    halt(1).

probe_row(row("https://probe.example/avatar.jpg", "bio", "did:plc:handle-change-probe",
              "new-handle-probe.bsky.social", "Probe")).

run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),
    open_pg_connection(C0),
    pg_query_simple(C0, "DELETE FROM weaving_user WHERE usr_twitter_id = 'did:plc:handle-change-probe'", _),
    pg_query_simple(C0, "INSERT INTO weaving_user (usr_id, usr_twitter_id, usr_twitter_username, usr_user_name, usr_username_canonical, url, usr_status, usr_email) VALUES (990101, 'did:plc:handle-change-probe', 'old-handle-probe.bsky.social', 'old-handle-probe.bsky.social', 'old-handle-probe.bsky.social', 'old-handle-probe.bsky.social', false, '')", _),
    probe_row(Row),

    insert_list_items_if_not_exists(pg_session(C0, C1), Row, _),
    expect_count(C1,
        "SELECT count(*)::text FROM weaving_user WHERE usr_twitter_id = 'did:plc:handle-change-probe'",
        "1", "rows for the DID after the import under its new handle"),
    expect_count(C1,
        "SELECT count(*)::text FROM weaving_user WHERE usr_id = 990101 AND usr_twitter_username = 'new-handle-probe.bsky.social' AND usr_user_name = 'new-handle-probe.bsky.social' AND usr_username_canonical = 'new-handle-probe.bsky.social' AND url = 'new-handle-probe.bsky.social'",
        "1", "existing row moved to the new handle"),

    insert_list_items_if_not_exists(pg_session(C1, C2), Row, _),
    expect_count(C2,
        "SELECT count(*)::text FROM weaving_user WHERE usr_twitter_id = 'did:plc:handle-change-probe' AND usr_twitter_username = 'new-handle-probe.bsky.social'",
        "1", "rows for the DID after a second import under the same handle"),

    pg_query_simple(C2, "DELETE FROM weaving_user WHERE usr_twitter_id = 'did:plc:handle-change-probe'", _),
    close_pg_connection(C2),
    format("[OK] member_new_handle_test: the member's row moved to its new handle, no second row~n", []),
    halt(0).
