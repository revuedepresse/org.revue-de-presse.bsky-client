:- module(sigsegv_min_repro, [run/0]).

:- use_module(library(assoc)).
:- use_module(library(format)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(serialization/json)).

:- use_module('../../src/stream', [read_stream/2]).
:- use_module('../../src/serialization', [pairs_to_assoc/2]).
:- use_module('../../src/infrastructure/repository/repository_status', [
    exists_by_uri_t/3
]).

/*
Absolute-minimum reproducer for the patched-VM SIGSEGV.

Strips every contributing step we have not yet ruled out:

  PARSE=1  call phrase(json_chars(pairs(_)), Chars)  (default)
  PARSE=0  skip parsing -- just read_stream then drop the body
  WALK=1   pairs_to_assoc + get_assoc(feed, ...)     (default)
  WALK=0   skip the walk
  EXTRACT=1 insert_record_args on post 0             (default)
  EXTRACT=0 hardcode all fields, never touch the JSON

Then calls Q0 (exists_by_uri_t SELECT) + Q1 (repository_status
INSERT) with the captured-or-synthetic handle/URI. The bug
established so far:
  http_open + read_stream + json_chars + Q0 + Q1 + minimal JSON
  + synthetic fields = SIGSEGV.

This file lets us see whether json_chars, the walk, and the
extract are each load-bearing in the trigger, or whether only
http_open's stream lifecycle and the Q0/Q1 pair matter.

URL / DB env mirror the chain bisector.
*/

env_or_die(Name) :-
    (   getenv(Name, _) -> true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

env_chars_default(Name, Default, Value) :-
    (   getenv(Name, V) -> Value = V ; Value = Default ).

env_bool(Name) :-
    getenv(Name, Value),
    ( Value = "1" ; Value = "true" ),
    !.

default_url("http://127.0.0.1:8080/feed.json").

run :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),

    default_url(DefaultUrl),
    env_chars_default("FEED_URL", DefaultUrl, URL),

    format("[..] http_open ~s~n", [URL]),
    http_open(URL, Stream, []),
    read_stream(Stream, BodyChars),
    length(BodyChars, BodyLen),
    format("[..] body chars=~w~n", [BodyLen]),

    (   env_bool("PARSE")
    ->  format("[..] PARSE=1, running phrase(json_chars(pairs(_)), Chars)~n", []),
        phrase(json_chars(pairs(Pairs)), BodyChars),
        (   env_bool("WALK")
        ->  format("[..] WALK=1, pairs_to_assoc + get_assoc(feed)~n", []),
            pairs_to_assoc(Pairs, Assoc),
            get_assoc(feed, Assoc, _Posts)
        ;   format("[..] WALK=0~n", [])
        )
    ;   format("[..] PARSE=0 (json_chars skipped)~n", [])
    ),

    % Hardcoded fields -- no dependency on Bluesky data.
    Handle = "min.example",
    URI = "at://min/post/0",
    Text = "hello",
    DisplayName = "M",
    AuthorAvatar = "https://x/a",
    CreatedAt = "2024-01-01T00:00:00.000Z",
    empty_assoc(Payload),

    format("[..] Q0: exists_by_uri_t(~s, ~s, T)~n", [Handle, URI]),
    exists_by_uri_t(Handle, URI, T0),
    format("[..] Q0 returned T=~q~n", [T0]),

    format("[..] Q1: repository_status:insert/3~n", []),
    repository_status:insert(
        row(DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt),
        StatusResult, RecordId
    ),
    format("[OK] survived: Q1 returned ~q record_id=~q~n", [StatusResult, RecordId]),
    halt(0).

:- initialization(run).
