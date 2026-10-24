:- module(list_item_from_event_test, [main/0]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(serialization/json)).

:- use_module('../src/assert', [
    assert/4,
    run_test_suite/2
]).
:- use_module('../src/infrastructure/repository/repository_list_item', [
    from_event/2
]).
:- use_module('../src/serialization', [
    char_code_at/2,
    wrapped_pairs_to_assoc/2
]).

/*
Each list item is stored as an event whose payload from_event/2 decodes
into the member's weaving_user row. A profile whose "associated" object
has no "chat" (only "activitySubscription") must still decode, or the
member never gets a weaving_user row and its posts are never ranked.
*/

%% item_payload(+Item, -Handle, -Payload)
%
% Encodes a getList item the way event_getListItem's onGetListItem/3 does.
item_payload(Item, Handle, Payload) :-
    Item = pairs(UnwrappedPairs),
    wrapped_pairs_to_assoc(Item, Assoc),
    get_assoc(subject, Assoc, Subject),
    get_assoc(handle, Subject, Handle),
    write_term_to_chars(UnwrappedPairs, [quoted(true),double_quotes(true)], Chars),
    chars_utf8bytes(Chars, Utf8Bytes),
    maplist(char_code_at, Utf8Bytes, Utf8BytesPayload),
    chars_base64(Utf8BytesPayload, Payload, []).

undecodable(Item, Handles0, Handles) :-
    item_payload(Item, Handle, Payload),
    (   catch(from_event(Payload, row(_, _, _, Handle, _)), _, false)
    ->  Handles = Handles0
    ;   Handles = [Handle|Handles0]
    ).

fixture_items(Items) :-
    phrase_from_file(seq(Body), 'tests/fixtures/getList-coop-medias.json'),
    phrase(json_chars(pairs(Pairs)), Body),
    member(string("items")-list(Items), Pairs).

test(Spec) :-
    Spec = 'list_item_from_event:every_coop_medias_member_decodes',
    fixture_items(Items),
    foldl(undecodable, Items, [], Undecodable),
    assert(Spec, true, [], Undecodable).
test(Spec) :-
    Spec = 'list_item_from_event:a_member_without_chat_settings_decodes',
    fixture_items(Items),
    member(Item, Items),
    item_payload(Item, "vert.eco", Payload),
    (   from_event(Payload, row(_, _, Did, Handle, DisplayName))
    ->  Outcome = decoded(Did, Handle, DisplayName)
    ;   Outcome = failed
    ),
    assert(Spec, true, decoded("did:plc:ittis6mfcchi54qe7m7d5mun", "vert.eco", "Vert, le média 💚"), Outcome).

main :-
    Specs = [
        'list_item_from_event:every_coop_medias_member_decodes',
        'list_item_from_event:a_member_without_chat_settings_decodes'
    ],
    run_test_suite(list_item_from_event_test, Specs).

:- initialization(main).
