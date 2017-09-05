-module(seq_handler).

%% Woody handler

-include_lib("dmsl/include/dmsl_sequences_thrift.hrl").

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-define(NS, <<"sequences">>).

%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()}.

handle_function('GetCurrent', [SequenceId], Context, _Opts) ->
    handle_result(seq_machine:get_state(?NS, SequenceId, Context));

handle_function('GetNext', [SequenceId], Context, _Opts) ->
    handle_result(seq_machine:call(?NS, SequenceId, Context)).

handle_result({ok, Result}) ->
    {ok, seq_marshalling:unmarshal(Result)}.