:- module(sigsegv_pgquery_repro, [run/0]).

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(crypto)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(reif)).

:- use_module('../../deps/postgresql-prolog/postgresql', [
    connect/6,
    query/4
]).
:- use_module('../../src/infrastructure/pg/connection', [
    pg_query/3
]).
:- use_module('../../src/infrastructure/repository/repository_status', [
    exists_by_uri_t/3,
    insert/3
]).

/*
The absolute minimum reproducer.

No HTTP, no JSON-DCG, no captured fixture, no repository_status
layer. Just postgresql-prolog's extended-protocol client running a
SELECT followed by an INSERT against the test Postgres. If this
crashes, the bug is reachable via a 30-line .pl file that talks to
nothing but PG.

Sequence
--------
  1. connect/6
  2. Q0: SELECT count(*) ... WHERE ust_hash = $1
  3. Q1: INSERT INTO weaving_status ... ON CONFLICT DO NOTHING RETURNING ust_id

Env vars
--------
  DATABASE_HOST / PORT / USERNAME / PASSWORD / DB_NAME

Set up the test PG first via `make compose-up`, then truncate
`weaving_status` between runs so Q1 always hits the NEW path:
  PGPASSWORD=test psql -h 127.0.0.1 -p 55432 -U test \\
    -d revue_de_presse_test -c \\
    "TRUNCATE TABLE public.weaving_status RESTART IDENTITY CASCADE;"
*/

env_chars(Name, Value) :- getenv(Name, Value).

char_code_inv(Code, Char) :- char_code(Char, Code).

env_number(Name, Number) :-
    getenv(Name, Chars),
    number_chars(Number, Chars).

% Same query shape as repository_status.pl:579-585.
count_sql([
    'S','E','L','E','C','T',' ','c','o','u','n','t','(','*',')',' ',
    'F','R','O','M',' ','p','u','b','l','i','c','.','w','e','a','v','i','n','g','_','s','t','a','t','u','s',' ',
    'W','H','E','R','E',' ','u','s','t','_','h','a','s','h',' ','=',' ','$','1'
]).

% Same query shape as repository_status.pl:233-245 (status_insert_sql).
insert_sql([
    'I','N','S','E','R','T',' ','I','N','T','O',' ','p','u','b','l','i','c','.','w','e','a','v','i','n','g','_','s','t','a','t','u','s',' ','(',
    'u','s','t','_','h','a','s','h',',',' ','u','s','t','_','n','a','m','e',',',' ','u','s','t','_','f','u','l','l','_','n','a','m','e',',',' ',
    'u','s','t','_','t','e','x','t',',',' ','u','s','t','_','a','v','a','t','a','r',',',' ','u','s','t','_','a','p','i','_','d','o','c','u','m','e','n','t',',',' ',
    'u','s','t','_','s','t','a','t','u','s','_','i','d',',',' ','u','s','t','_','a','c','c','e','s','s','_','t','o','k','e','n',',',' ',
    'i','s','_','p','u','b','l','i','s','h','e','d',',',' ','u','s','t','_','c','r','e','a','t','e','d','_','a','t',
    ')',' ','V','A','L','U','E','S',' ','(','$','1',',',' ','$','2',',',' ','$','3',',',' ','$','4',',',' ','$','5',',',' ','$','6',',',' ','$','7',',',' ','$','8',',',' ',
    '$','9',':',':','b','o','o','l','e','a','n',',',' ','$','1','0',')',' ',
    'O','N',' ','C','O','N','F','L','I','C','T',' ','(','u','s','t','_','h','a','s','h',')',' ','D','O',' ','N','O','T','H','I','N','G',' ',
    'R','E','T','U','R','N','I','N','G',' ','u','s','t','_','i','d',':',':','t','e','x','t'
]).

run :-
    env_chars("DATABASE_USERNAME", User),
    env_chars("DATABASE_PASSWORD", Pass),
    env_chars("DATABASE_HOST", Host),
    env_number("DATABASE_PORT", Port),
    env_chars("DATABASE_DB_NAME", DB),

    format("[..] connect ~s@~s:~w/~s~n", [User, Host, Port, DB]),
    connect(User, Pass, Host, Port, DB, Conn),
    format("[..] connected~n", []),

    % Use crypto_data_hash like the production path does
    % (repository_status -> client.pl:hash/2). If a hardcoded hex
    % chars list survives but a crypto-computed hash crashes, the
    % bug correlates with library(crypto)'s arena allocations.
    (   getenv("USE_CRYPTO", "1")
    ->  format("[..] computing Hash via crypto_data_hash~n", []),
        append(["h", "|", "u"], UniqueIdentifier),
        crypto_data_hash(UniqueIdentifier, Hash, [algorithm(sha256)])
    ;   format("[..] using hardcoded Hash (no crypto)~n", []),
        Hash = "deadbeef00000000000000000000000000000000000000000000000000000000"
    ),

    count_sql(SelectSQL),
    format("[..] Q0: SELECT count(*) WHERE ust_hash = $1~n", []),
    (   getenv("USE_EXISTS", "1")
    ->  format("[..] (using repository_status:exists_by_uri_t/3 directly)~n", []),
        exists_by_uri_t("h", "u", _T0),
        R0 = '<via exists_by_uri_t>'
    ;   getenv("USE_PROJECT_PG", "1")
    ->  format("[..] (using project pg_query/3 with bb_put cache)~n", []),
        pg_query(SelectSQL, [Hash], R0)
    ;   query(Conn, SelectSQL, [Hash], R0)
    ),
    format("[..] Q0 returned ~q~n", [R0]),

    % Mimic encode_field_value from client.pl:83-87 between Q0 and Q1.
    % The COMPLETE chain: write_term_to_chars + chars_utf8bytes +
    % maplist + chars_base64. This is the b02e0c47 commit's named
    % candidate ("the FFI surface used for chars_utf8bytes/2").
    (   getenv("USE_ENCODE", "1")
    ->  format("[..] full encode_field_value chain~n", []),
        empty_assoc(EmptyAssoc),
        write_term_to_chars(EmptyAssoc, [quoted(true), double_quotes(true)], Quoted),
        chars_utf8bytes(Quoted, Utf8Bytes),
        maplist(char_code_inv, Utf8Bytes, FieldUtf8Bytes),
        chars_base64(FieldUtf8Bytes, EncodedFieldValue, []),
        length(EncodedFieldValue, EncLen),
        format("[..] encoded to ~w base64 chars~n", [EncLen])
    ;   format("[..] skipping encode_field_value path~n", [])
    ),
    % if_/3 from reif -- exists_by_uri_t closes with if_(Count = 0, ...)
    (   getenv("USE_REIF", "1")
    ->  format("[..] reif if_/3 call~n", []),
        if_(0 = 0, true, true)
    ;   true
    ),

    insert_sql(InsertSQL),
    format("[..] Q1: INSERT ... ON CONFLICT DO NOTHING RETURNING~n", []),
    InsertParams = [Hash, "h", "h", "t", "https://x/a", "{}", "at://x/y",
                    "tok", "true", "2024-01-01T00:00:00Z"],
    (   getenv("USE_REPO_INSERT", "1")
    ->  format("[..] (using repository_status:insert/3)~n", []),
        empty_assoc(EmptyPayload),
        insert(row("X", "h", "t", "https://x/a", EmptyPayload, "at://x/y", "2024-01-01T00:00:00.000Z"),
               R1, _RecordId)
    ;   getenv("USE_PROJECT_PG", "1")
    ->  pg_query(InsertSQL, InsertParams, R1)
    ;   query(Conn, InsertSQL, InsertParams, R1)
    ),
    format("[OK] Q1 returned ~q -- survived~n", [R1]),
    halt(0).

:- initialization(run).
