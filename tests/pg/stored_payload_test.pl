:- module(stored_payload_test, [run_test/0]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(pio)).

:- use_module('../../src/infrastructure/pg/connection', [
    close_pg_connection/1,
    open_pg_connection/1,
    pg_query_simple/3
]).
:- use_module('../../src/infrastructure/repository/repository_list', [
    by_list_uri_or_throw/3,
    insert/3
]).
:- use_module('../../src/app/bsky/graph/getList', [payload/2]).

%% decodes(+C0, -C, +Label, +ListURI, +BodyChars, -Assoc)
%
% Stores BodyChars through getList's payload/2, then decodes it back.
decodes(C0, C, Label, ListURI, BodyChars, Assoc) :-
    getList:payload(BodyChars, Payload),
    repository_list:insert(pg_session(C0, C1), row(ListURI, Payload), _),
    catch(
        repository_list:by_list_uri_or_throw(pg_session(C1, C), list_uri(ListURI), Assoc),
        E,
        ( format("[KO] ~s: decoding the stored payload threw ~w~n", [Label, E]), halt(1) )
    ).

%% run_test/0
%
% A stored list payload must decode back to the list, whatever its
% members' profiles contain:
%
%   1. the real coop-medias.org getList response (48 members; one
%      display name holds a no-break space, which the Prolog string
%      the payload is stored as escapes as \xa0\);
%   2. a list whose description holds escaped double quotes.
run_test :-
    open_pg_connection(C0),
    pg_query_simple(C0,
        "DELETE FROM publishers_list_collected_event WHERE list_name LIKE 'at://stored-payload-probe%'",
        _),

    phrase_from_file(seq(Fixture), 'tests/fixtures/getList-coop-medias.json'),
    once(append([Before, "at://did:plc:hsvsr3edhbdymtjrg5cifbce/app.bsky.graph.list/3mymhj5akx32w", After], Fixture)),
    append([Before, "at://stored-payload-probe-coop", After], CoopBody),
    decodes(C0, C1, "coop-medias.org list", "at://stored-payload-probe-coop", CoopBody, CoopAssoc),
    (   get_assoc(list, CoopAssoc, CoopList),
        get_assoc(name, CoopList, "coop-medias.org"),
        get_assoc(items, CoopAssoc, CoopItems),
        length(CoopItems, 48)
    ->  format("[ok] the coop-medias.org payload decodes to its 48 members~n", [])
    ;   format("[KO] the coop-medias.org payload decoded to ~w~n", [CoopAssoc]), halt(1) ),

    QuotedBody = "{\"list\":{\"uri\":\"at://stored-payload-probe-quotes\",\"name\":\"probe\",\"description\":\"say \\\"hi\\\"\"},\"items\":[]}",
    decodes(C1, C2, "escaped quotes", "at://stored-payload-probe-quotes", QuotedBody, QuotedAssoc),
    (   get_assoc(list, QuotedAssoc, QuotedList),
        get_assoc(description, QuotedList, "say \"hi\"")
    ->  format("[ok] a description with escaped quotes decodes intact~n", [])
    ;   format("[KO] the escaped-quotes payload decoded to ~w~n", [QuotedAssoc]), halt(1) ),

    close_pg_connection(C2),
    format("[OK] stored_payload_test passed~n", []),
    halt(0).
