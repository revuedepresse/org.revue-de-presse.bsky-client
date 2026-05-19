:- module(logger, [
    log_debug/1,
    log_error/1,
    log_info/1
]).

:- use_module(library(os)).
:- use_module(library(si)).

above_error_level :-
    getenv("LOG_LEVEL", "critical")
    ;   getenv("LOG_LEVEL", "alert")
    ;   getenv("LOG_LEVEL", "emergency").

above_info_level :-
    getenv("LOG_LEVEL", "notice")
    ;   getenv("LOG_LEVEL", "warning")
    ;   getenv("LOG_LEVEL", "error")
    ;   above_error_level.

above_debug_level :-
    getenv("LOG_LEVEL", "info")
    ;   above_info_level.

log([]) :- nl.
log([Message|Rest]) :-
    once(write_message(Message)),
    log(Rest).

write_message(Message) :- atomic_si(Message), write(Message).
write_message(Message) :-
    \+ atomic_si(Message),
    integer(Message),
    number_chars(Message, MessageChars),
    atom_chars(AtomicMessage, MessageChars),
    write(AtomicMessage).
write_message(Message) :-
    \+ atomic_si(Message),
    \+ integer(Message),
    chars_si(Message),
    atom_chars(AtomicMessage, Message),
    write(AtomicMessage),
    write(' ').
write_message(Message) :-
    \+ atomic_si(Message),
    \+ integer(Message),
    \+ chars_si(Message),
    list_si(Message),
    write(Message),
    write(' ').
write_message(Message) :-
    \+ atomic_si(Message),
    \+ integer(Message),
    \+ chars_si(Message),
    \+ list_si(Message),
    writeq(Message),
    write(' ').

log_debug(Messages) :-
    once(debug(Messages)).

    debug(_) :-
        above_debug_level.

    debug(Messages) :-
        log(Messages).

log_info(Messages) :-
    once(info(Messages)).

    info(_) :-
        above_info_level.

    info(Messages) :-
        log(Messages).

log_error(Messages) :-
    once(error(Messages)).

    error(_) :-
        above_error_level.

    error(Messages) :-
        log(Messages).
