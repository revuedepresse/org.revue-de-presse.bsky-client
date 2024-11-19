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
    ;   getenv("LOG_LEVEL", "emergency")
    ;   \+ getenv("LOG_LEVEL", _).

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
    (   atomic_si(Message)
    ->  LogMessage = Message,
        write(LogMessage)
    ;   integer(Message),
        number_chars(Message,MessageChars),
        atom_chars(AtomicMessage, MessageChars),
        LogMessage = AtomicMessage,
        write(LogMessage)
    ;   chars_si(Message),
        atom_chars(AtomicMessage,Message),
        LogMessage = AtomicMessage,
        write(LogMessage)
    ;   list_si(Message),
        LogMessage = Message,
        write(LogMessage)
    ;   LogMessage = Message,
        writeq(LogMessage)
    ),
    write(' '),
    log(Rest).

log_debug(_) :-
    \+ getenv("LOG_LEVEL", "debug")
    ;   above_debug_level.
log_debug(Messages) :-
    log(Messages).

log_info(_) :-
    \+ getenv("LOG_LEVEL", "info")
    ;   above_info_level.
log_info(Messages) :-
    log(Messages).

log_error(_) :-
    \+ getenv("LOG_LEVEL", "error")
    ;   above_error_level.
log_error(Messages) :-
    log(Messages).
