-module(seq_machine).

-include_lib("dmsl/include/dmsl_state_processing_thrift.hrl").

-export([call/3]).
-export([get_state/3]).

%% State processor

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-define(NIL, {nl, #msgpack_Nil{}}).

-type id()          :: dmsl_base_thrift:'ID'().
-type ns()          :: dmsl_base_thrift:'Namespace'().
-type context()     :: woody_context:ctx().
-type aux_state()   :: dmsl_state_processing_thrift:'AuxState'().

%%

-spec call(ns(), id(), context()) ->
    {ok, aux_state()}.

call(Ns, Id, Context) ->
    Descriptor = prepare_descriptor(Ns, {id, Id}, #'HistoryRange'{}),
    case call_automaton('Call', [Descriptor, ?NIL], Context) of
        {ok, Response} ->
            {ok, Response};
        {error, #'MachineNotFound'{}} ->
            {ok, ok} = start(Ns, Id, Context),
            call(Ns, Id, Context)
    end.

-spec get_state(ns(), id(), context()) ->
    {ok, aux_state()}.

get_state(Ns, Id, Context) ->
    Descriptor = prepare_descriptor(Ns, {id, Id}, #'HistoryRange'{}),
    case call_automaton('GetMachine', [Descriptor], Context) of
        {ok, #'Machine'{aux_state = AuxState}} ->
            {ok, AuxState};
        {error, #'MachineNotFound'{}} ->
            {ok, ok} = start(Ns, Id, Context),
            get_state(Ns, Id, Context)
    end.

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
    {ok, AuxState} = seq_handler:init(),
    Change = #'MachineStateChange'{
        events = [],
        aux_state = AuxState
    },
    {ok, #'SignalResult'{
        change = Change,
        action = #'ComplexAction'{}
    }};

handle_function('ProcessCall', [#'CallArgs'{machine = #'Machine'{aux_state = CurrentAuxState}}], _Context, _Opts) ->
    {ok, NextAuxState} = seq_handler:process_call(CurrentAuxState),
    Change = #'MachineStateChange'{
        events = [],
        aux_state = NextAuxState
    },
    {ok, #'CallResult'{
        change = Change,
        action = #'ComplexAction'{},
        response = NextAuxState
    }}.