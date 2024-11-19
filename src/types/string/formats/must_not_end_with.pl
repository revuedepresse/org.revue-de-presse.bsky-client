:- module(must_not_end_with, [must_not_end_with/2]).

:- use_module(library(lists)).
:- use_module(must_not_start_with, [must_not_start_with/2]).

% must_not_end_with(+Subject, +Char).
must_not_end_with(Subject, Char) :-
    reverse(Subject, ReversedSubject),
    catch(
        must_not_start_with(ReversedSubject, Char),
        error_must_not_start_with(_),
        throw(error_must_not_end_with(Char))
    ).
