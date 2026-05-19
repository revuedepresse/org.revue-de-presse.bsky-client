:- module(must_not_end_with, [must_not_end_with/2]).

/**
Assert that a char list does not end with a given character.

Implemented by reversing the subject and delegating to
[[must_not_start_with]]. Rethrows
`error_must_not_start_with(_)` as
`error_must_not_end_with(_)` so error messages stay aligned
with caller intent.
*/

:- use_module(library(lists)).
:- use_module(must_not_start_with, [must_not_start_with/2]).

%% must_not_end_with(+Subject, +Char)
%
% Succeed iff the last element of `Subject` is not `Char`;
% throw `error_must_not_end_with/1` otherwise.
must_not_end_with(Subject, Char) :-
    reverse(Subject, ReversedSubject),
    catch(
        must_not_start_with(ReversedSubject, Char),
        error_must_not_start_with(_),
        throw(error_must_not_end_with(Char))
    ).
