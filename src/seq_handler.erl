-module(seq_handler).

%% Woody handler

-include_lib("dmsl/include/dmsl_sequences_thrift.hrl").

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%% Machine callbacks

-export([init/0]).
-export([process_call/1]).

%%

-define(NS, <<"sequences">>).
-define(INIT, 0).

-type msgpack_value() :: dmsl_msgpack_thrift:'Value'().

%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()}.

handle_function('GetCurrent', [SequenceId], Context, _Opts) ->
    handle_result(seq_machine:get_state(?NS, SequenceId, Context));

handle_function('GetNext', [SequenceId], Context, _Opts) ->
    handle_result(seq_machine:call(?NS, SequenceId, Context)).

handle_result({ok, Result}) ->
    {ok, unmarshal(Result)}.

%% Machine callbacks

-spec init() ->
    {ok, msgpack_value()}.
init() ->
    {ok, marshal(?INIT)}.

-spec process_call(msgpack_value()) ->
    {ok, msgpack_value()}.
process_call(CurrentValue) ->
    {ok, marshal(unmarshal(CurrentValue) + 1)}.

%% Marshalling

marshal(Int) when is_integer(Int) ->
    {i, Int}.

unmarshal({i, Int}) ->
    Int.