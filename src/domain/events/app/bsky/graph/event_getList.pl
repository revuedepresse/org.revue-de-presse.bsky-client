:- module(event_getList, [
    onGetList/2
]).

:- use_module('../../../../../infrastructure/repository/repository_lists', [
    insert/2
]).
:- use_module('../../../../../logger', [
    log_debug/1,
    log_error/1,
    log_info/1
]).

onGetList(ListUri, Payload) :-
    catch(
        once((
            insert(
                row(
                    ListUri,
                    Payload
                ),
                InsertionResult
            ),
            log_debug([list_insertion_result(InsertionResult)])
        )),
        E,
        log_error([list_insertion_result(E)])
    ).