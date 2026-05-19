:- module(event_getProfile, [
    onGetProfile/1
]).

/**
Domain event handler for `app.bsky.actor.getProfile`.

Called once per author observed in a list. Looks up the
existing `publishers_list` row for the `(ListURI, DID)`
combination; on `cannot_read_rows_selection`, inserts a new
row instead. Surfaces a `pre_existing_publisher/1` throw when
the row is already there so the upstream pipeline can short-
circuit re-processing.
*/

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(serialization/json)).

:- use_module('../../../../../infrastructure/repository/repository_list', [
    by_list_uri_or_throw/2
]).
:- use_module('../../../../../infrastructure/repository/repository_publisher', [
    by_criteria/3,
    insert/2
]).
:- use_module('../../../../../logger', [
    log_debug/1,
    log_error/1,
    log_info/1
]).
:- use_module('../../../../../serialization', [
    char_code_at/2,
    pairs_to_assoc/2,
    to_json_chars/2
]).
:- use_module('../../../../../stream', [
    writeln/1,
    writeln/2
]).

%% onGetProfile(+Props)
%
% Handle a profile observation. `Props` is a
% `props(ListURI, FollowersCount, FollowsCount, DID, Payload)`
% term. Reads the existing publisher row by
% `(ListURI, DID)`; on miss inserts a new row, otherwise
% throws `pre_existing_publisher/1`.
onGetProfile(props(ListURI, FollowersCount, FollowsCount, DID, Payload)) :-
    catch(
        on_get_profile_check_existing(ListURI, DID),
        Cause,
        if_(
            Cause = cannot_read_rows_selection,
            on_get_profile_insert(ListURI, FollowersCount, FollowsCount, DID, Payload),
            on_get_profile_propagate(Cause)
        )
    ).

on_get_profile_check_existing(ListURI, DID) :-
    once(repository_publisher:by_criteria(list_uri(ListURI), did(DID), Rows)),
    once(report_existing_publisher_row(Rows)).

report_existing_publisher_row(Rows) :-
    nth0(0, Rows, FirstRow),
    get_assoc(screen_name, FirstRow, PreExistingDID),
    log_debug(['Pre-existing record for did':PreExistingDID]),
    throw(pre_existing_publisher(PreExistingDID)).
report_existing_publisher_row(_).

on_get_profile_insert(ListURI, FollowersCount, FollowsCount, DID, Payload) :-
    once((
        by_list_uri_or_throw(list_uri(ListURI), ListUriPairs),
        get_assoc(list_id, ListUriPairs, ListId),
        repository_publisher:insert(
            row(
                ListId,
                ListURI,
                FollowersCount,
                FollowsCount,
                DID,
                Payload
            ),
            InsertionResult
        ),
        log_info([list_insertion_result(InsertionResult)])
    )).

on_get_profile_propagate(pre_existing_publisher(PreExistingDID)) :-
    throw(pre_existing_publisher(PreExistingDID)).
