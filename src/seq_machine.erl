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
    integer().

get_next(Id, Context) ->
    {ok, AuxState} = call_automaton_with_lazy_start('Call', Id, [?NIL], Context),
    get_sequence_value(AuxState).

-spec get_current(id(), context()) ->
    integer().

get_current(Id, Context) ->
    {ok, #'Machine'{aux_state = AuxState}} = call_automaton_with_lazy_start('GetMachine', Id, Context),
    get_sequence_value(AuxState).

get_sequence_value(AuxState) ->
    unmarshal(AuxState).

%%

start(Id, Context) ->
    case call_automaton('Start', [?NS, Id, ?NIL], Context) of
        {ok, _} ->
            ok;
        {exception, #'MachineAlreadyExists'{}} ->
            ok
    end.

call_automaton(Function, Id, Args, Context) ->
    Descriptor = construct_descriptor({id, Id}),
    call_automaton(Function, [Descriptor|Args], Context).

call_automaton(Function, Args, Context) ->
    Request = {{dmsl_state_processing_thrift, 'Automaton'}, Function, Args},
    {ok, URL} = application:get_env(sequences, automaton_service_url),
    Opts = #{url => URL, event_handler => {woody_event_handler_default, undefined}},
    case woody_client:call(Request, Opts, Context) of
        {ok, _} = Ok ->
            Ok;
        {exception, Exception} ->
            {exception, Exception}
    end.

call_automaton_with_lazy_start(Function, Id, Context) ->
    call_automaton_with_lazy_start(Function, Id, [], Context).

call_automaton_with_lazy_start(Function, Id, Args, Context) ->
    case call_automaton(Function, Id, Args, Context) of
        {ok, _} = Ok ->
            Ok;
        {exception, #'MachineNotFound'{}} ->
            ok = start(Id, Context),
            call_automaton(Function, Id, Args, Context)
    end.

construct_descriptor(Ref) ->
    #'MachineDescriptor'{
        ns = ?NS,
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
    marshal(?INIT).

process_call(CurrentValue) ->
    NextValue = unmarshal(CurrentValue) + 1,
    marshal(NextValue).

%% Marshalling

marshal(Int) when is_integer(Int) ->
    {arr, [{i, 1}, {i, Int}]}.

unmarshal({arr, [{i, 1},  {i, Int}]}) ->
    Int.