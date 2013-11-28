-module(simple_distributed_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common/include/test.hrl").

-define(APPS,      [ sasl ]).
-define(CNT_NODES, 3).
-define(TIMEOUT,   100).

%% =============================================================================
%%  Common
%% =============================================================================

setup()->
    error_logger:tty(false),
    tools:make_distrib("test_node", shortnames),
    ?START_APPS(
        ?APPS, [
                {sasl, [ {sasl_error_logger, {file, "test.log"}} ]}
        ]),
    ok.

cleanup(_)->
    ?STOP_APPS(?APPS),
    tools:stop_distrib(),
    error_logger:tty(true),
    timer:sleep(1000),
    ok.

%% =============================================================================
%%  Tests
%% =============================================================================
stop_master_test_()->
    {setup, fun setup/0, fun cleanup/1,
        {timeout, 30, fun() ->
                Nodes = [ ec_test_tools:start_node(Id, ?CNT_NODES, ?TIMEOUT) || Id <- lists:seq(1,?CNT_NODES,1)],
                timer:sleep(750),
                ?assertEqual(?CNT_NODES, length(Nodes)),
                ?assertEqual(?CNT_NODES, length(nodes())),

                Masters = [ec_test_tools:get_master(Node) || Node <- lists:sort(nodes())],
                ShouldBeMasterId = 3,
                ec_test_tools:assert_master(ShouldBeMasterId, Masters),

                distributed_node:stop_applications(ec_test_tools:node_name(3), [echo_cluster]),
                timer:sleep(750),
                NewMasters = [ec_test_tools:get_master(Node) || Node <- lists:sort(nodes())],
                NewShouldBeMasterId = 2,
                ec_test_tools:assert_master(NewShouldBeMasterId, NewMasters),

                distributed_node:stop_nodes(Nodes),
                ok
        end}
        }.

shuffle_stop_test_()->
    {setup, fun setup/0, fun cleanup/1,
        {timeout, 30, fun() ->
                Nodes = [ ec_test_tools:start_node(Id, ?CNT_NODES, ?TIMEOUT) || Id <- ec_test_tools:shuffle_list(lists:seq(1,?CNT_NODES,1))],
                timer:sleep(750),
                ?assertEqual(?CNT_NODES, length(Nodes)),
                ?assertEqual(?CNT_NODES, length(nodes())),

                Masters = [ec_test_tools:get_master(Node) || Node <- lists:sort(nodes())],
                ShouldBeMasterId = 3,
                ec_test_tools:assert_master(ShouldBeMasterId, Masters),

                distributed_node:stop_applications(ec_test_tools:node_name(3), [echo_cluster]),
                timer:sleep(750),
                NewMasters = [ec_test_tools:get_master(Node) || Node <- lists:sort(nodes())],
                NewShouldBeMasterId = 2,
                ec_test_tools:assert_master(NewShouldBeMasterId, NewMasters),

                distributed_node:stop_nodes(Nodes),
                ok
        end}
        }.

%% =============================================================================
%%  Helpers
%% =============================================================================
