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
uses is looked up by its current handle, found missing and inserted
again with the same DID. weaving_user is unique on (usr_twitter_id,
usr_twitter_username), so the member gets a row under its current
handle, which the nightly top 10 needs to select its posts, and the
old row stays.
*/

env_or_die(Name) :-
    (   getenv(Name, _)
    ->  true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

count(C, SQL, Count) :-
    pg_query_simple(C, SQL, Reply),
    (   Reply = data(_, [[Count]])
    ->  true
    ;   Count = Reply ).

run_test :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),
    open_pg_connection(C0),
    pg_query_simple(C0, "DELETE FROM weaving_user WHERE usr_twitter_id = 'did:plc:handle-change-probe'", _),
    pg_query_simple(C0, "INSERT INTO weaving_user (usr_id, usr_twitter_id, usr_twitter_username, usr_status, usr_email) VALUES (990101, 'did:plc:handle-change-probe', 'old-handle-probe.bsky.social', false, '')", _),
    insert_list_items_if_not_exists(
        pg_session(C0, C1),
        row("https://probe.example/avatar.jpg", "bio", "did:plc:handle-change-probe",
            "new-handle-probe.bsky.social", "Probe"),
        _
    ),
    count(C1, "SELECT count(*)::text FROM weaving_user WHERE usr_twitter_id = 'did:plc:handle-change-probe' AND usr_twitter_username = 'new-handle-probe.bsky.social'", New),
    count(C1, "SELECT count(*)::text FROM weaving_user WHERE usr_twitter_id = 'did:plc:handle-change-probe' AND usr_twitter_username = 'old-handle-probe.bsky.social'", Old),
    pg_query_simple(C1, "DELETE FROM weaving_user WHERE usr_twitter_id = 'did:plc:handle-change-probe'", _),
    close_pg_connection(C1),
    (   New == "1", Old == "1"
    ->  format("[OK] member_new_handle_test: the member has a row under its new handle, the old one stays~n", []),
        halt(0)
    ;   format("[KO] member_new_handle_test: rows under the new handle: ~q, under the old one: ~q~n", [New, Old]),
        halt(1) ).
