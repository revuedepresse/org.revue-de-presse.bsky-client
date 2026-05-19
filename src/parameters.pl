:- module(parameters, [
    ensures_query_parameter/1,
    extract_single_required_parameter/2
]).

/**
Parameter validation for XRPC client modules.

Each generated XRPC stub calls the helpers here once per
request to assert that the matching OpenAPI parameter is
present in `query` position and that exactly one required
parameter is declared — enough validation to keep the URL
builder honest without re-implementing a JSON-schema
validator.
*/

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

%% ensures_query_parameter(+Parameter)
%
% Succeed only when `Parameter`'s `in` field is the chars
% `"query"`; throw `error_unsupported_parameter/1` otherwise.
% Used before building the request URL because the URL builder
% only knows how to emit `?name=value` form.
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

%% extract_single_required_parameter(+Parameters, -RequiredParameter)
%
% Reduce a list of OpenAPI parameter pairs to the single one
% flagged `required: true`, or throw
% `error_more_than_one_parameter/1` if zero or several are.
extract_single_required_parameter(Parameters, RequiredParameter) :-
    maplist(unwrap_pairs, Parameters, UnwrappedParameters),
    maplist(pairs_to_assoc, UnwrappedParameters, ParametersAssocs),
    include(is_required, ParametersAssocs, Requirements),

    ensures_single_parameter_is_required(Requirements),

    Requirements = [RequiredParameter].