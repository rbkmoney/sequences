-module(seq_client).

-export([new_context/0]).
-export([get_current/2]).
-export([get_next/2]).

-type context() :: woody_context:ctx().

-spec get_current(binary(), context()) -> integer().

get_current(SeqId, Context) ->
    call('GetCurrent', [SeqId], Context).

-spec get_next(binary(), context()) -> integer().

get_next(SeqId, Context) ->
    call('GetNext', [SeqId], Context).

-spec new_context() -> context().

new_context() ->
    woody_context:new().

call(Function, Args, Context) ->
    Call = {{dmsl_sequences_thrift, 'Sequences'}, Function, Args},
    Opts = #{
        url => <<"http://sequences:8022/v1/sequences">>,
        event_handler => {woody_event_handler_default, undefined}
    },
    case woody_client:call(Call, Opts, Context) of
        {ok, Response} ->
            Response;
        {exception, Exception} ->
            throw(Exception)
    end.