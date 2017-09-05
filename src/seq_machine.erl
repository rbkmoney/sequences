-module(seq_machine).

-include_lib("dmsl/include/dmsl_state_processing_thrift.hrl").

-export([get_current/3]).
-export([get_next/3]).

%% State processor

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-define(NIL, {nl, #msgpack_Nil{}}).
-define(INIT, 0).

-type id()          :: dmsl_base_thrift:'ID'().
-type ns()          :: dmsl_base_thrift:'Namespace'().
-type context()     :: woody_context:ctx().

%%

-type msgpack_value() :: dmsl_msgpack_thrift:'Value'().

-spec get_next(ns(), id(), context()) ->
    {ok, msgpack_value()}.

get_next(Ns, Id, Context) ->
    handle_result(get_next_(Ns, Id, Context)).

get_next_(Ns, Id, Context) ->
    Descriptor = prepare_descriptor(Ns, {id, Id}, #'HistoryRange'{}),
    case call_automaton('Call', [Descriptor, ?NIL], Context) of
        {ok, Response} ->
            {ok, Response};
        {error, #'MachineNotFound'{}} ->
            {ok, ok} = start(Ns, Id, Context),
            get_next_(Ns, Id, Context)
    end.

-spec get_current(ns(), id(), context()) ->
    {ok, msgpack_value()}.

get_current(Ns, Id, Context) ->
    handle_result(get_current_(Ns, Id, Context)).

get_current_(Ns, Id, Context) ->
    Descriptor = prepare_descriptor(Ns, {id, Id}, #'HistoryRange'{}),
    case call_automaton('GetMachine', [Descriptor], Context) of
        {ok, #'Machine'{aux_state = AuxState}} ->
            {ok, AuxState};
        {error, #'MachineNotFound'{}} ->
            {ok, ok} = start(Ns, Id, Context),
            get_current_(Ns, Id, Context)
    end.

handle_result({ok, Result}) ->
    {ok, unmarshal(Result)}.

%%

start(Ns, Id, Context) ->
    call_automaton('Start', [Ns, Id, ?NIL], Context).

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

prepare_descriptor(NS, Ref, Range) ->
    #'MachineDescriptor'{
        ns = NS,
        ref = Ref,
        range = Range
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
    marshal(?INIT).

process_call(CurrentValue) ->
    marshal(unmarshal(CurrentValue) + 1).

marshal(Int) when is_integer(Int) ->
    {i, Int}.

unmarshal({i, Int}) ->
    Int.