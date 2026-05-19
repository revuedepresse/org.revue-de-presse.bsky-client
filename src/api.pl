:- module('api', [endpoint_spec_pairs/2]).

/**
Goal-expanded resolver for XRPC endpoint specs.

Each XRPC client module under `src/{app,chat,com}/...` calls
`endpoint_spec_pairs/2` once. At load time `user:goal_expansion/2`
inspects the calling source file, derives the matching spec at
`doc/endpoints/<dotted-endpoint-id>.value.json`, parses it through
the JSON DCG, and rewrites the call so the spec is statically
embedded in the client. The runtime call is therefore a constant
lookup — no I/O at request time.
*/

:- use_module(library(charsio)).
:- use_module(library(dif)).
:- use_module(library(files)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(si)).
:- use_module(library(crypto)).
:- use_module(library(serialization/json)).

:- use_module('logger', [
    log_info/1,
    log_debug/1
]).
:- use_module('stream', [
    read_stream/2,
    read_stream/3
]).

:- meta_predicate(endpoint_spec_pairs(2, +)).

%% endpoint_spec_pairs(-Pairs, +IsExpanded)
%
% Unifies `Pairs` with the parsed OpenAPI spec for the XRPC endpoint
% defined in the calling module. The runtime clause is a stub; the
% real work happens at load time through `user:goal_expansion/2`,
% which rewrites the call to embed the parsed spec literally.
endpoint_spec_pairs(_Pairs, _IsExpanded).

%% Replace Target char with Replacement char.
%
%% replace(+Target, +Replacement, +Char, +In, -Out).
replace(Target, Replacement, Char, In, Out) :-
    if_(
        dif(Char, Target),
        append([In, [Char]], Out),
        append([In, [Replacement]], Out)
    ).

%% Expand goal so that Json spec is phrased.
user:goal_expansion(Goal, ExpandedGoal) :-
    Goal = (endpoint_spec_pairs(Pairs, IsExpanded)),
    prolog_load_context(source, SourceAtom),

    atom_si(SourceAtom),

    if_(
       dif(IsExpanded, false),
       fail,
       (expand_endpoint_spec_pairs(Pairs, SourceAtom, ExpandedGoal))
    ).

extract_endpoint_or_throw(CanonicalSourceChars, RootDir, Endpoint) :-
    append([Prefix, ".pl"], CanonicalSourceChars),
    append([RootDir, "src/", Endpoint], Prefix).
extract_endpoint_or_throw(CanonicalSourceChars, _, _) :-
    \+ append([_, ".pl"], CanonicalSourceChars),
    throw(cannot_find_endpoint(CanonicalSourceChars)).

%% replaces '/' with '.' in endpoint, producing endpoint spec basename
%
%% expand_endpoint_spec_pairs(+SourceAtom, -Spec.
expand_endpoint_spec_pairs(Pairs, SourceAtom, endpoint_spec_pairs(Pairs, true)) :-
    atom_si(SourceAtom),
    atom_chars(SourceAtom, SourceChars),
    path_canonical(SourceChars, CanonicalSourceChars),

    extract_endpoint_or_throw(CanonicalSourceChars, RootDir, Endpoint),

    foldl(replace('/', '.'), Endpoint, "", Basename),
    append([RootDir, "doc/endpoints/", Basename, ".value.json"], EndpointSpecJson),

    once((
        file_exists(EndpointSpecJson)
    ;   throw(file_does_not_exist(EndpointSpecJson)) )),

    open(EndpointSpecJson, read, Stream, [type(text)]),
    read_stream(Stream, EndpointSpecJsonContent),
    phrase(json_chars(pairs(Pairs)), EndpointSpecJsonContent),
    log_debug([pairs:Pairs]).
