-module(seq_machine).

-include_lib("dmsl/include/dmsl_state_processing_thrift.hrl").

-export([get_current/2]).
-export([get_next/2]).

%% State processor

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-define(NS, <<"sequences">>).
-define(NIL, {nl, #msgpack_Nil{}}).
-define(INIT, 0).

-type id()          :: dmsl_base_thrift:'ID'().
-type context()     :: woody_context:ctx().

%%

-spec get_next(id(), context()) ->
    {ok, integer()}.

get_next(Id, Context) ->
    handle_result(get_next_(Id, Context)).

get_next_(Id, Context) ->
    Descriptor = construct_descriptor(?NS, {id, Id}),
    case call_automaton('Call', [Descriptor, ?NIL], Context) of
        {ok, Response} ->
            {ok, Response};
        {error, Error} ->
            handle_error({fun(X1, X2) -> get_next_(X1, X2) end, Id, Context}, Error)
    end.

-spec get_current(id(), context()) ->
    {ok, integer()}.

get_current(Id, Context) ->
    handle_result(get_current_(Id, Context)).

get_current_(Id, Context) ->
    Descriptor = construct_descriptor(?NS, {id, Id}),
    case call_automaton('GetMachine', [Descriptor], Context) of
        {ok, #'Machine'{aux_state = AuxState}} ->
            {ok, AuxState};
        {error, Error} ->
            handle_error({fun(X1, X2) -> get_current_(X1, X2) end, Id, Context}, Error)
    end.

handle_result({ok, Result}) ->
    {ok, unmarshal(Result)}.

handle_error({Function, Id, Context}, #'MachineAlreadyExists'{}) ->
    Function(Id, Context);
handle_error({Function, Id, Context}, #'MachineNotFound'{}) ->
    {ok, ok} = start(Id, Context),
    Function(Id, Context).

%%

start(Id, Context) ->
    call_automaton('Start', [?NS, Id, ?NIL], Context).

call_automaton(Function, Args, Context) ->
    Request = {{dmsl_state_processing_thrift, 'Automaton'}, Function, Args},
    {ok, URL} = application:get_env(sequences, automaton_service_url),
    Opts = #{url => URL, event_handler => {woody_event_handler_default, undefined}},
    case woody_client:call(Request, Opts, Context) of
        {ok, _} = Ok ->
            Ok;
        {exception, Exception} ->
            {error, Exception}
    end.

construct_descriptor(NS, Ref) ->
    #'MachineDescriptor'{
        ns = NS,
        ref = Ref,
        range = #'HistoryRange'{}
    }.

%%

-type func() :: 'ProcessSignal' | 'ProcessCall'.

-spec handle_function(func(), woody:args(), context(), woody:options()) ->
    {ok, term()}.

handle_function('ProcessSignal', [#'SignalArgs'{signal = {init, _}}], _Context, _Opts) ->
    {ok, #'SignalResult'{
        change = construct_change(init()),
        action = #'ComplexAction'{}
    }};

handle_function('ProcessCall', [#'CallArgs'{machine = #'Machine'{aux_state = CurrentAuxState}}], _Context, _Opts) ->
    NextAuxState = process_call(CurrentAuxState),
    {ok, #'CallResult'{
        change = construct_change(NextAuxState),
        action = #'ComplexAction'{},
        response = NextAuxState
    }}.

construct_change(State) ->
    #'MachineStateChange'{
        events = [],
        aux_state = State
    }.

init() ->
    marshal([1, ?INIT]).

process_call(CurrentValue) ->
    marshal([1, unmarshal(CurrentValue) + 1]).

marshal([1, Int]) when is_integer(Int) ->
    {arr, [{i, 1}, {i, Int}]}.

unmarshal({arr, [{i, 1}, {i, Int}]}) ->
    Int.