:- module(event_getListItem, [
    onGetListItem/2
]).

/**
Domain event handler for each item in `app.bsky.graph.getList`.

Encodes the item's pair list to a base64 payload, then either
logs the existing event row for the same handle or inserts a
new one and seeds the `weaving_user` record. Used once per
list member during the fan-out from
[[event_getList#onGetList]].
*/

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(serialization/json)).

:- use_module('../../../../../infrastructure/repository/repository_list', [
    insert/2
]).
:- use_module('../../../../../infrastructure/repository/repository_list_item', [
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

%% onGetListItem(+ItemAssoc, +Pairs)
%
% Handle a single list member. `ItemAssoc` is the assoc form
% of the item; `Pairs` is the raw JSON-DCG pair list, which is
% serialised and base64-encoded for storage. Existing rows are
% logged; missing rows are inserted into
% `member_profile_collected_event` and `weaving_user`.
onGetListItem(ItemAssoc, pairs(UnwrappedPairs)) :-
    write_term_to_chars(UnwrappedPairs, [quoted(true),double_quotes(true)], Chars),

    chars_utf8bytes(Chars, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, Utf8BytesPayload),
    chars_base64(Utf8BytesPayload, Payload, []),

    get_assoc('subject', ItemAssoc, Subject),
    get_assoc(handle, Subject, ScreenName),

    catch(
        on_get_list_item_log_existing(ScreenName, Payload),
        E,
        if_(
            E = cannot_read_rows_selected_by(_),
            on_get_list_item_insert(ScreenName, Payload),
            log_error([unexpected_error(E)])
        )
    ).

on_get_list_item_log_existing(ScreenName, Payload) :-
    once(repository_list_item:event_by_screen_name(ScreenName, Rows)),
    once(log_existing_list_item_payload(Rows, Payload)).

log_existing_list_item_payload(Rows, Payload) :-
    nth0(0, Rows, FirstRow),
    get_assoc(payload, FirstRow, Payload),
    from_event(Payload, row(_,_,_,Handle,_)),
    writeln('list item value related to "subject" key':Handle, true).
log_existing_list_item_payload(_, _).

on_get_list_item_insert(ScreenName, Payload) :-
    once(repository_list_item:insert(
        row(ScreenName, Payload),
        InsertionResult
    )),
    from_event(Payload, RowFromPayload),
    insert_list_items_if_not_exists(RowFromPayload, _),
    writeln(screen_name:ScreenName, true),
    writeln(insertion_result:InsertionResult, true),
    log_info([payload:Payload]).
