:- module(connection, [
    pg_connection/1,
    pg_query/3,
    pg_query_simple/2,
    pg_transaction/1
]).

:- use_module(library(lists)).
:- use_module(library(iso_ext), [bb_get/2, bb_put/2]).

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

%% pg_connection(-Conn)
%
% Returns the process-wide PostgreSQL connection, opening it lazily on
% first call (via SCRAM-SHA-256 if the server demands it). Subsequent
% calls return the same connection term, so a Scryer run pays the
% handshake cost once. Stored in the global blackboard because the
% wire client's connection term wraps an opaque stream.
pg_connection(Conn) :-
    cached_connection(Conn).
pg_connection(Conn) :-
    \+ cached_connection(_),
    database_username(User),
    database_password(Pass),
    database_host(Host),
    database_port(PortChars),
    number_chars(Port, PortChars),
    database_db_name(DB),
    connect(User, Pass, Host, Port, DB, Conn),
    bb_put(pg_conn, Conn).

cached_connection(Conn) :-
    catch(bb_get(pg_conn, Conn), _, fail),
    Conn \= [].

%% pg_query(+SQL, +Params, -Result)
%
% Extended protocol with bind parameters. Use placeholders $1, $2, ...
% in SQL and pass values in Params (each value a list of chars, or the
% atom `null`). Numbers must be number_chars-encoded by the caller.
%
% Result is one of:
%   - data([])              for INSERT/UPDATE/DELETE with no RETURNING.
%   - data([Row, ...])      for SELECT or INSERT ... RETURNING.
%   - error(ErrorString)    on a server-reported error.
%
% Each row is a list of fields; each field is a list of UTF-8 chars
% or the atom `null` for SQL NULL.
pg_query(SQL, Params, Result) :-
    pg_connection(Conn),
    query(Conn, SQL, Params, Result).

%% pg_query_simple(+SQL, -Result)
%
% Simple protocol (no bind params). Use only with SQL that contains no
% caller-controlled values. Result is one of:
%   - ok                    for statements with no result rows.
%   - data(Headers, Rows)   for SELECT-style statements.
%   - []                    for an empty query.
%   - error(ErrorString)    on a server-reported error.
pg_query_simple(SQL, Result) :-
    pg_connection(Conn),
    query(Conn, SQL, Result).

%% pg_transaction(:Goal)
%
% Runs Goal inside a transaction. Commits on success, ROLLBACKs on throw
% and re-raises. Goal must be a callable goal; module-qualify it if it
% lives in another module.
pg_transaction(Goal) :-
    pg_query_simple("BEGIN", _),
    catch(
        ( call(Goal),
          pg_query_simple("COMMIT", _) ),
        E,
        ( pg_query_simple("ROLLBACK", _),
          throw(E) )
    ).
