:- module('api', [endpoint_spec_pairs/2]).

:- use_module(library(dif)).
:- use_module(library(files)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(si)).
:- use_module(library(serialization/json)).

:- use_module(stream, [read_stream/3]).
:- meta_predicate(endpoint_spec_pairs(2, +)).

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
user:goal_expansion(Term, ExpandedTerm) :-
    Term = (endpoint_spec_pairs(Pairs, IsExpanded)),
    if_(
        dif(IsExpanded, true),
        (prolog_load_context(source, SourceAtom),

        atom_chars(SourceAtom, SourceChars),
        append([RootDir, "./src/", Endpoint, ".pl"], SourceChars),

        foldl(replace('/', '.'), Endpoint, "", FilenamePrefix),
        append([RootDir, "doc/endpoints/", FilenamePrefix, ".value.json"], EndpointSpecJson),

        once((
            file_exists(EndpointSpecJson)
        ;   throw(file_does_not_exist(EndpointSpecJson)) )),

        open(EndpointSpecJson, read, Stream, [type(text)]),
        read_stream(Stream, [], EndpointSpecJsonContent),
        phrase(json_chars(pairs(Pairs)), EndpointSpecJsonContent),

        ExpandedTerm = (endpoint_spec_pairs(Pairs, true))),
        fail
    ).