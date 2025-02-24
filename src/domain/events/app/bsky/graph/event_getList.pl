:- module(event_getList, [
    onGetList/2
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
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
    writeln/1,
    writeln/2
]).

%% Handling onGetList Event
%
%% onGetList(+ListUri, -Payload)
onGetList(ListUri, Payload) :-
    catch(
        (once(repository_lists:by_list_uri(ListUri, Rows)),
        ( ( nth0(0, Rows, FirstRow),
            get_assoc(payload, FirstRow, FirstRowPayload) )
        ->  chars_base64(Utf8BytesPayload, FirstRowPayload, []),
            maplist(char_code, Utf8BytesPayload, Utf8Bytes),
            chars_utf8bytes(PayloadChars, Utf8Bytes),

            % Removing surrounding double quotes
            % before converting in-between string to JSON Chars
            % so that it could be parsed with phrase/3
            append([Prefix, ['"']], PayloadChars),
            append([['"'], Suffix], Prefix),
            to_json_chars(Suffix, JSONChars),

            phrase(json_chars(pairs(Pairs)), JSONChars, []),
            maplist(get_key, Pairs, Keys),
            pairs_to_assoc(Pairs, _Assoc),
            writeln('list keys':Keys, true)
        ;   true )),
        cannot_read_rows_selected_by(_Selector),
        catch(
            once((
                repository_lists:insert(
                    row(
                        ListUri,
                        Payload
                    ),
                    InsertionResult
                ),
                log_info([list_insertion_result(InsertionResult)])
            )),
            E,
            log_error([list_insertion_result(E)])
        )
    ).

get_key(string(Key)-_Value, Key).
