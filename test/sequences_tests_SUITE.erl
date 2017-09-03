-module(sequences_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

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
        {service_urls, #{
            'Sequences' => <<"sequences:8022/v1/sequences">>
        }},
        {automaton_service_url, "http://machinegun:8022/v1/automaton"}
    ]),
    [{suite_apps, Apps} | C].

-spec end_per_suite(config()) -> term().
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

%% tests

-spec get_current(term()) -> term().
get_current(_C) ->
    SeqId = get_sequence_id(),
    0 = call('GetCurrent', [SeqId]),
    0 = call('GetCurrent', [SeqId]),
    _ = call('GetNext', [SeqId]),
    1 = call('GetCurrent', [SeqId]).

-spec get_next(term()) -> term().
get_next(_C) ->
    SeqId = get_sequence_id(),
    1 = call('GetNext', [SeqId]),
    2 = call('GetNext', [SeqId]).

%%

call(Function, Args) ->
    Call = {{dmsl_sequences_thrift, 'Sequences'}, Function, Args},
    Opts = #{
        url => <<"sequences:8022/v1/sequences">>,
        event_handler => {woody_event_handler_default, undefined}
    },
    Context = woody_context:new(),
    case woody_client:call(Call, Opts, Context) of
        {ok, Response} ->
            Response;
        {exception, Exception} ->
            throw(Exception)
    end.

get_sequence_id() ->
    integer_to_binary(erlang:system_time(micro_seconds)).