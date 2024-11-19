:- module(at_identifier, [
    is_valid_at_identifier/1
]).

:- use_module(did_identifier, [is_valid_did_identifier/1]).
:- use_module(handle, [is_valid_handle/1]).
:- use_module(must_start_with, [must_start_with/2]).

% `at-identifier`: either a [Handle](https://atproto.com/specs/handle) or a [DID](https://atproto.com/specs/did)
%
% is_valid_at_identifier(+Subject).
is_valid_at_identifier(Subject) :-
    catch(
        \+ must_start_with(Subject, "did:"),
        error_must_start_with(_),
        is_valid_handle(Subject)
    ).
is_valid_at_identifier(Subject) :-
    is_valid_did_identifier(Subject).
