:- module(sigsegv_databisect_repro, [run/0]).

:- use_module(library(assoc)).
:- use_module(library(format)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(serialization/json)).

:- use_module('../../src/stream', [read_stream/2]).
:- use_module('../../src/serialization', [pairs_to_assoc/2]).
:- use_module('../../src/domain/events/app/bsky/feed/event_getAuthorFeed', [
    insert_record_args/9
]).
:- use_module('../../src/infrastructure/repository/repository_status', [
    exists_by_uri_t/3
]).

/*
Data-shape bisector. Pipeline mirrors sigsegv_chainbisect_repro.pl at
CHAIN_LEVEL=2 (Q0 + Q1, the minimum that crashes). After
insert_record_args/9 has decoded the captured post, individual
fields are optionally overridden with synthetic values controlled
by per-field SYNTH_<FIELD> env vars:

  SYNTH_DISPLAYNAME=1   override DisplayName with a short atom-y list
  SYNTH_HANDLE=1        override Handle with "synth.example"
  SYNTH_TEXT=1          override Text with "hello"
  SYNTH_AVATAR=1        override Avatar with a short HTTPS URL
  SYNTH_PAYLOAD=1       override Payload with empty assoc
  SYNTH_URI=1           override URI with a fresh "at://synth/<rand>"
                        (also implies a fresh Hash since hash = sha256
                        of handle||"|"||URI)
  SYNTH_CREATEDAT=1     override CreatedAt with "2024-01-01T00:00:00.000Z"

Setting all SYNTH_*=1 lets us check whether the bug is reachable
purely on the structural shape of an http_open response, with no
captured content at all. Each SYNTH_X=0 keeps the captured value.
*/

env_or_die(Name) :-
    (   getenv(Name, _) -> true
    ;   format("[KO] missing env var ~s~n", [Name]), halt(2) ).

env_chars_default(Name, Default, Value) :-
    (   getenv(Name, V) -> Value = V ; Value = Default ).

env_number_default(Name, Default, Number) :-
    (   getenv(Name, Chars) -> number_chars(Number, Chars) ; Number = Default ).

env_bool(Name) :- getenv(Name, "1").

default_url("http://127.0.0.1:8080/feed.json").

load_feed_posts_via_http(Posts) :-
    default_url(DefaultUrl),
    env_chars_default("FEED_URL", DefaultUrl, URL),
    RequestHeaders = [
        'User-Agent'('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36')
    ],
    Options = [
        method(get),
        status_code(_StatusCode),
        request_headers(RequestHeaders),
        headers(_ResponseHeaders)
    ],
    format("[..] http_open ~s~n", [URL]),
    http_open(URL, Stream, Options),
    read_stream(Stream, BodyChars),
    phrase(json_chars(pairs(ResponsePairs)), BodyChars),
    pairs_to_assoc(ResponsePairs, FeedAssoc),
    get_assoc(feed, FeedAssoc, Posts).

% Override fields when env says so. Default: pass through captured value.
override(Name, Original, Replacement, Final) :-
    (   env_bool(Name)
    ->  format("[..] SYNTH ~w override ENABLED~n", [Name]),
        Final = Replacement
    ;   Final = Original
    ).

empty_assoc(A) :- list_to_assoc([], A).

run :-
    maplist(env_or_die,
            ["DATABASE_HOST","DATABASE_PORT","DATABASE_USERNAME",
             "DATABASE_PASSWORD","DATABASE_DB_NAME"]),
    env_number_default("POST_INDEX", 0, Index),
    format("[..] POST_INDEX=~w~n", [Index]),

    load_feed_posts_via_http(Posts),
    nth0(Index, Posts, Post),

    insert_record_args(
        Post,
        DisplayName0, Handle0, Text0, AuthorAvatar0, Payload0, URI0, CreatedAt0,
        likes(_)-reposts(_)
    ),
    format("[..] original handle=~s uri=~s~n", [Handle0, URI0]),

    override("SYNTH_DISPLAYNAME", DisplayName0, "Synth", DisplayName),
    override("SYNTH_HANDLE",      Handle0,      "synth.example", Handle),
    override("SYNTH_TEXT",        Text0,        "hello", Text),
    override("SYNTH_AVATAR",      AuthorAvatar0, "https://example.com/a.png", AuthorAvatar),
    empty_assoc(EmptyAssoc),
    override("SYNTH_PAYLOAD",     Payload0,     EmptyAssoc, Payload),
    override("SYNTH_URI",         URI0,         "at://synth/post/0", URI),
    override("SYNTH_CREATEDAT",   CreatedAt0,   "2024-01-01T00:00:00.000Z", CreatedAt),

    format("[..] effective handle=~s uri=~s~n", [Handle, URI]),

    format("[..] Q0: exists_by_uri_t~n", []),
    exists_by_uri_t(Handle, URI, T0),
    format("[..] Q0 returned T=~q~n", [T0]),

    format("[..] Q1: repository_status:insert/3~n", []),
    repository_status:insert(
        row(DisplayName, Handle, Text, AuthorAvatar, Payload, URI, CreatedAt),
        StatusResult, RecordId
    ),
    format("[OK] Q1 returned ~q record_id=~q -- survived chain~n",
           [StatusResult, RecordId]),
    halt(0).

:- initialization(run).
