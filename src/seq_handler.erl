-module(seq_handler).

%% Woody handler

-include_lib("dmsl/include/dmsl_sequences_thrift.hrl").

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()}.

handle_function('GetCurrent', [SequenceId], Context, _Opts) ->
    Value = seq_machine:get_current(SequenceId, Context),
    {ok, Value};

handle_function('GetNext', [SequenceId], Context, _Opts) ->
    Value = seq_machine:get_next(SequenceId, Context),
    {ok, Value}.