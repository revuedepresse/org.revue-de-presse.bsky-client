:- module(event_getListItem, [
    onGetListItem/2
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(serialization/json)).

:- use_module('../../../../../infrastructure/repository/repository_lists', [
    insert/2
]).
:- use_module('../../../../../infrastructure/repository/repository_list_items', [
    event_by_screen_name/2,
    from_event/2,
    record_by_screen_name/2,
    insert_list_items_if_not_exists/2,
    insert/2,
    insert_record/2
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
    char_code_at/2,
    pairs_to_assoc/2,
    to_json_chars/2,
    writeln/1,
    writeln/2
]).

%% Handling onGetListItem Event
%
%% onGetListItem(+ItemAssoc, +Pairs)
onGetListItem(ItemAssoc, pairs(UnwrappedPairs)) :-
    write_term_to_chars(UnwrappedPairs, [quoted(true),double_quotes(true)], Chars),

    chars_utf8bytes(Chars, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, Utf8BytesPayload),
    chars_base64(Utf8BytesPayload, Payload, []),

    get_assoc('subject', ItemAssoc, Subject),
    get_assoc(handle, Subject, ScreenName),

    catch(
        (once(repository_list_items:event_by_screen_name(ScreenName, Rows)),
        ( ( nth0(0, Rows, FirstRow),
            get_assoc(payload, FirstRow, Payload) )
        ->
            from_event(Payload, row(_,_,_,Handle,_)),
            write_term('list item value related to "subject" key':Handle, [double_quotes(true)]), nl
        ;   true )),
        E,
        if_(
            E = cannot_read_rows_selected_by(_),
            (once(repository_list_items:insert(
                row(ScreenName, Payload),
                InsertionResult
            )),

            from_event(Payload, RowFromPayload),
            insert_list_items_if_not_exists(
                RowFromPayload,
                _
            ),

            writeln(screen_name:ScreenName, true),
            writeln(insertion_result:InsertionResult, true),

            log_info([payload:Payload])),
            log_error([unexpected_error(E)])
        )
    ).
