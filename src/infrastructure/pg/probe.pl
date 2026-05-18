:- module(probe, [run/0]).

:- use_module(library(format)).
:- use_module(library(os)).

:- use_module('../../../deps/postgresql-prolog/postgresql', [connect/6, query/3]).

env_or_die(Name, Value) :-
    (   getenv(Name, V)
    ->  Value = V
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

%% run/0
%
% Read-only probe of the database pointed to by DATABASE_* env vars.
% Reports server version, authenticated user, password storage method,
% and whether SSL is enabled. No data is written.
run :-
    env_or_die("DATABASE_HOST", Host),
    env_or_die("DATABASE_PORT", PortChars),
    number_chars(Port, PortChars),
    env_or_die("DATABASE_USERNAME", User),
    env_or_die("DATABASE_PASSWORD", Pass),
    env_or_die("DATABASE_DB_NAME", DB),

    format("[..] connecting to ~s:~d as ~s -> ~s~n", [Host, Port, User, DB]),
    connect(User, Pass, Host, Port, DB, Conn),
    format("[ok] connected~n", []),

    query(Conn,
          "SELECT version(), current_user, current_setting('password_encryption'), current_setting('ssl')",
          Result),
    print_probe(Result).

print_probe(data(_Headers, [[Version, CurrentUser, Encryption, Ssl]])) :-
    format("[INFO] server_version      : ~s~n", [Version]),
    format("[INFO] current_user        : ~s~n", [CurrentUser]),
    format("[INFO] password_encryption : ~s~n", [Encryption]),
    format("[INFO] ssl                 : ~s~n", [Ssl]),
    halt(0).
print_probe(Other) :-
    format("[KO] unexpected probe result: ~w~n", [Other]),
    halt(1).
