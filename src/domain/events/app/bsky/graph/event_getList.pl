:- module(event_getList, [
    onGetList/3
]).

/**
Domain event handler for `app.bsky.graph.getList`.

Decodes the most recent persisted payload for the list AT-URI
if one exists; on a miss writes a new
`publishers_list_collected_event`. Used at the start of every
worker run to capture the latest list-level metadata before
fanning out to individual list items.

Takes a `pg_session(In, Out)` compound as its first argument and
threads it through the repository calls.
*/

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(serialization/json)).

:- use_module('../../../../../infrastructure/repository/repository_list', [
    by_list_uri/3,
    insert/3
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

%% onGetList(+Session, +ListUri, +Payload)
%
% Handle a list snapshot for `ListUri` carrying `Payload`. On
% an existing event row, decode and log its payload; on a
% miss (`cannot_read_rows_selected_by/1`), insert a fresh row
% and log the outcome. The catch undoes the lookup's bindings,
% so the insert starts again from the session's input
% connection.
onGetList(pg_session(C0, C), ListUri, Payload) :-
    catch(
        on_get_list_decode_existing(pg_session(C0, C), ListUri),
        cannot_read_rows_selected_by(_Selector),
        on_get_list_insert(pg_session(C0, C), ListUri, Payload)
    ).

on_get_list_decode_existing(Session, ListUri) :-
    once(repository_list:by_list_uri(Session, ListUri, Rows)),
    once(decode_first_row_payload(Rows)).

decode_first_row_payload(Rows) :-
    nth0(0, Rows, FirstRow),
    get_assoc(payload, FirstRow, FirstRowPayload),
    chars_base64(Utf8BytesPayload, FirstRowPayload, []),
    maplist(char_code, Utf8BytesPayload, Utf8Bytes),
    chars_utf8bytes(PayloadChars, Utf8Bytes),
    append([Prefix, ['"']], PayloadChars),
    append([['"'], Suffix], Prefix),
    to_json_chars(Suffix, JSONChars),
    phrase(json_chars(pairs(Pairs)), JSONChars, []),
    maplist(get_key, Pairs, Keys),
    pairs_to_assoc(Pairs, _Assoc),
    writeln('list keys':Keys).
decode_first_row_payload(_).

on_get_list_insert(pg_session(C0, C), ListUri, Payload) :-
    catch(
        once((
            repository_list:insert(
                pg_session(C0, C),
                row(ListUri, Payload),
                InsertionResult
            ),
            log_info([list_insertion_result(InsertionResult)])
        )),
        E,
        ( log_error([list_insertion_result(E)]), C = C0 )
    ).

get_key(string(Key)-_Value, Key).
