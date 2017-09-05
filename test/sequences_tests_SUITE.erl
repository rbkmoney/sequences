-module(sequences_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_current/1]).
-export([get_next/1]).

-include_lib("dmsl/include/dmsl_sequences_thrift.hrl").

%% tests descriptions

-type config() :: [{atom(), term()}].

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-spec all() -> [atom()].
all() ->
    [
        get_current,
        get_next
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    Apps = genlib_app:start_application_with(lager, [
        {async_threshold, 1},
        {async_threshold_window, 0},
        {error_logger_hwm, 600},
        {suppress_application_start_stop, true},
        {handlers, [
            {lager_common_test_backend, warning}
        ]}
    ]) ++ genlib_app:start_application_with(sequences, [
        {automaton_service_url, "http://machinegun:8022/v1/automaton"}
    ]),
    [{suite_apps, Apps} | C].

-spec end_per_suite(config()) -> term().
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

%% tests

-spec init_per_testcase(atom(), config()) -> config().

init_per_testcase(_Name, C) ->
    Context = seq_client:new_context(),
    [{context, Context} | C].

-spec end_per_testcase(atom(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

-spec get_current(term()) -> term().
get_current(C) ->
    Context = proplists:get_value(context, C),
    SeqId = get_sequence_id(),
    0 = seq_client:get_current(SeqId, Context),
    0 = seq_client:get_current(SeqId, Context),
    _ = seq_client:get_next(SeqId, Context),
    1 = seq_client:get_current(SeqId, Context).

-spec get_next(term()) -> term().
get_next(C) ->
    Context = proplists:get_value(context, C),
    SeqId = get_sequence_id(),
    1 = seq_client:get_next(SeqId, Context),
    2 = seq_client:get_next(SeqId, Context).

%%

get_sequence_id() ->
    integer_to_binary(erlang:system_time(micro_seconds)).