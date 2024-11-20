:- module(logger, [log_info/1, log_error/1]).

log_info([]) :- nl.
log_info([Message|Rest]) :-
    writeq(Message),
    log_info(Rest).

log_error([]) :-
    nl,fail.
log_error([Message|Rest]) :-
    write(Message),
    log_error(Rest).
