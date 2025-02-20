:- module(event_getList, [
    onGetList/2,
    onGetListItem/2
]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(serialization/json)).

:- use_module('../../../../../infrastructure/repository/repository_lists', [
    insert/2
]).
:- use_module('../../../../../infrastructure/repository/repository_list_items', [
    by_screen_name/2,
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

onGetList(ListUri, Payload) :-
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
    ).

get_key(string(Key)-Value, Key).

onGetListItem(ItemAssoc, pairs(UnwrappedPairs)) :-
    write_term_to_chars(UnwrappedPairs, [quoted(true),double_quotes(true)], Chars),

    chars_utf8bytes(Chars, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, Utf8BytesPayload),
    chars_base64(Utf8BytesPayload, Payload, []),

    get_assoc('uri', ItemAssoc, URI),
    get_assoc('subject', ItemAssoc, Subject),

    assoc_to_keys(Subject, SubjectKeys),
    get_assoc(handle, Subject, ScreenName),

    catch(
        (once(repository_list_items:by_screen_name(ScreenName, Rows)),
        ( ( nth0(0, Rows, FirstRow),
            get_assoc(payload, FirstRow, Payload) )
        ->  chars_base64(Utf8BytesPayload, Payload, []),
            maplist(char_code, Utf8BytesPayload, Utf8Bytes),
            chars_utf8bytes(PayloadChars, Utf8Bytes),

            append(["pairs(", PayloadChars, ")."], CompleteReduction),
            read_from_chars(CompleteReduction, Term),
            Term = pairs(Pairs),
            maplist(get_key, Pairs, Keys),
            pairs_to_assoc(Pairs, Assoc),
            get_assoc(subject, Assoc, Subject),
            get_assoc(handle, Subject, Handle),
            writeq(subject:Handle), nl
        ;   true )),
        cannot_read_rows_selected_by(_),
        (
            once(repository_list_items:insert(
                row(ScreenName, Payload),
                InsertionResult
            )),

            write_term(screen_name:ScreenName, [double_quotes(true)]), nl,
            write_term(payload:Payload, [double_quotes(true)]), nl,
            write_term(insertion_result:InsertionResult, []), nl
        )
    ).

%    phrase(json_chars(pairs(Payload)), JChars),
%    write(payload:JChars), nl.
    %maplist(char_code, Utf8BytesPayload, DecodedBytes),
    %chars_utf8bytes(DecodedChars, DecodedBytes),
%    phrase(json_chars(pairs(UnwrappedPairs)), JsonChars).
    %once(write_term_to_chars(JsonChars, [quoted(false),double_quotes(true)], JChars)),
%    log_info([JChars]).