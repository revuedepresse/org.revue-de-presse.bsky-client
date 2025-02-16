:- module(parameters, [
    ensures_query_parameter/1,
    extract_single_required_parameter/2
]).

:- use_module(library(assoc)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(si)).

:- use_module(serialization, [
    pairs_to_assoc/2,
    unwrap_pairs/2
]).
:- use_module(filter, [include/3]).

ensures_query_parameter(Parameter) :-
    get_assoc(in, Parameter, Where),
    once((
        Where = "query"
    ;   throw(error_unsupported_parameter(Where)) )).

ensures_single_parameter_is_required(RequiredParameters) :-
    length(RequiredParameters, HowManyParameters),
    if_(
        dif(HowManyParameters, 1),
        throw(error_more_than_one_parameter(HowManyParameters)),
        true
    ).

extract_single_required_parameter(Parameters, RequiredParameter) :-
    maplist(unwrap_pairs, Parameters, UnwrappedParameters),
    maplist(pairs_to_assoc, UnwrappedParameters, ParametersAssocs),
    include(is_required, ParametersAssocs, Requirements),

    ensures_single_parameter_is_required(Requirements),

    Requirements = [RequiredParameter].