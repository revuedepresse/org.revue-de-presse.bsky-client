:- module(at_identifier, [
    is_valid_at_identifier/1
]).

/**
AT-identifier validator.

An [`at-identifier`](https://atproto.com/specs/at-uri-scheme)
is either a [Handle](https://atproto.com/specs/handle) or a
[DID](https://atproto.com/specs/did). The predicate here
dispatches on the `"did:"` prefix and defers to either
[[handle#is_valid_handle]] or
[[did_identifier#is_valid_did_identifier]].
*/

:- use_module(did_identifier, [is_valid_did_identifier/1]).
:- use_module(handle, [is_valid_handle/1]).
:- use_module(must_start_with, [must_start_with/2]).

%% is_valid_at_identifier(+Subject)
%
% Succeed iff `Subject` is a valid handle (when it does not
% start with `"did:"`) or a valid DID identifier.
is_valid_at_identifier(Subject) :-
    catch(
        \+ must_start_with(Subject, "did:"),
        error_must_start_with(_),
        is_valid_handle(Subject)
    ).
is_valid_at_identifier(Subject) :-
    is_valid_did_identifier(Subject).
