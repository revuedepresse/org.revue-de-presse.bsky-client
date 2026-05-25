:- module(connection, [
    open_pg_connection/1,
    close_pg_connection/1,
    pg_query/4,
    pg_query_simple/3,
    pg_transaction/2
]).

/**
Stateless Postgres connection primitives.

The previous version cached a single connection in the global
blackboard (bb_put/bb_get) so a Scryer run paid the handshake cost
once. The cache was the source of the silent-failure cascade: a
wire desync on any cached connection made every subsequent query
silently fail.

The new shape is purely functional: open_pg_connection/1 hands a
fresh `postgresql(Stream)` term to the caller; close_pg_connection/1
disposes of one. Higher layers thread the connection through their
calls inside a `pg_session(In, Out)` compound. There is no
module-level connection state.

pg_query/4 / pg_query_simple/3 take the connection term explicitly.
Their callers are responsible for threading it.
*/

:- use_module(library(lists)).

:- use_module('../../configuration', [
    database_db_name/1,
    database_host/1,
    database_password/1,
    database_port/1,
    database_username/1
]).

:- use_module('../../../deps/postgresql-prolog/postgresql', [
    connect/6,
    query/3,
    query/4
]).

%% open_pg_connection(-Conn)
%
% Open a fresh wire connection (SCRAM-SHA-256 handshake if the
% server demands it). Returns the `postgresql(Stream)` term.
% Throws on connect failure -- no silent failure.
open_pg_connection(Conn) :-
    database_username(User),
    database_password(Pass),
    database_host(Host),
    database_port(PortChars),
    number_chars(Port, PortChars),
    database_db_name(DB),
    connect(User, Pass, Host, Port, DB, Conn).

%% close_pg_connection(+Conn)
%
% Close the connection's underlying stream. Best-effort: errors
% during close are swallowed because callers want to dispose of
% the connection regardless of stream state (it may already be
% closed at peer if we're reacting to a wire silent failure).
close_pg_connection(postgresql(Stream)) :-
    catch(close(Stream), _, true).
close_pg_connection(Conn) :-
    Conn \= postgresql(_).

%% pg_query(+Conn, +SQL, +Params, -Result)
%
% Extended protocol with bind parameters on the given connection.
% Conn is the term returned by open_pg_connection/1. Use $1, $2,
% ... placeholders and pass values in Params (each value a list of
% chars, or the atom `null`).
%
% Result is one of:
%   - data([])              for INSERT/UPDATE/DELETE with no RETURNING.
%   - data([Row, ...])      for SELECT or INSERT ... RETURNING.
%   - error(ErrorString)    on a server-reported error.
%
% Throws wire_silent_failure(Detail) when the wire library detects
% a desync (EOF, unexpected response shape). Never silently fails.
pg_query(Conn, SQL, Params, Result) :-
    query(Conn, SQL, Params, Result).

%% pg_query_simple(+Conn, +SQL, -Result)
%
% Simple protocol (no bind params). Use only with SQL that contains
% no caller-controlled values.
pg_query_simple(Conn, SQL, Result) :-
    query(Conn, SQL, Result).

%% pg_transaction(+Conn, :Goal)
%
% Runs Goal inside a BEGIN/COMMIT transaction on the given
% connection. ROLLBACKs and re-raises on throw.
pg_transaction(Conn, Goal) :-
    pg_query_simple(Conn, "BEGIN", _),
    catch(
        ( call(Goal),
          pg_query_simple(Conn, "COMMIT", _) ),
        E,
        ( pg_query_simple(Conn, "ROLLBACK", _),
          throw(E) )
    ).
