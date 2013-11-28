-module(common_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common/include/test.hrl").

-define(APPS, [ sasl, echo_cluster ]).
-define(CNT_NODES, 7).

%% =============================================================================
%%  Common
%% =============================================================================

setup()->
    error_logger:tty(false),
    tools:make_distrib("test_node", shortnames),
    ?START_APPS(
        ?APPS, [
            {sasl, [ {sasl_error_logger, {file, atom_to_list(?MODULE)++".log"}} ]},
            {echo_cluster, [
                    {cluster, [{1, {127,0,0,1}, 5000}, {2, {127,0,0,2}, 5000}, {3, {127,0,0,3}, 5000}]},
                    {tick_interval_ms, 100} ]}
            ]
        ),
    ok.

cleanup(_)->
    ?STOP_APPS(?APPS),
    tools:stop_distrib(),
    error_logger:tty(true),
    ok.

%% =============================================================================
%%  Tests
%% =============================================================================
config_read_test_()->
    {setup, fun setup/0, fun cleanup/1,
        fun()->
                {ok, Nodes}    = application:get_env(echo_cluster, cluster),
                {ok, Interval} = application:get_env(echo_cluster, tick_interval_ms),
                ?assertMatch({true, true}, {is_list(Nodes), 0 < length(Nodes)}),
                ?assertMatch({true, true}, {is_integer(Interval), 0 < Interval}),
                ok
        end
        }.

%% =============================================================================
%%  Helpers
%% =============================================================================
