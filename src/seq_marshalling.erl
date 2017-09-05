-module(seq_marshalling).

-export([marshal/1]).
-export([unmarshal/1]).

-type msgpack_value() :: dmsl_msgpack_thrift:'Value'().

-spec marshal(integer()) ->
    msgpack_value().
marshal(Int) when is_integer(Int) ->
    {i, Int}.

-spec unmarshal(msgpack_value()) ->
    integer().
unmarshal({i, Int}) ->
    Int.