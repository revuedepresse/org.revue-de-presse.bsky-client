:- module(event_getProfile, [
    onGetProfile/1
]).

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

%% Handling onGetProfile Event
%
%% onGetProfile(+ListURI, +DID, +Payload)
onGetProfile(props(ListURI, FollowersCount, FollowsCount, DID, Payload)) :-
    catch(
        (once(repository_publisher:by_criteria(list_uri(ListURI), did(DID), Rows)),
        ( ( nth0(0, Rows, FirstRow),
            get_assoc(screen_name, FirstRow, PreExistingDID) )
        ->  log_debug(['Pre-existing record for did':PreExistingDID])
        ;   true )),
        Cause,
        if_(
            Cause = cannot_read_rows_selection,
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
            )),
            throw(pre_existing_publisher(PreExistingDID))
        )
    ).
