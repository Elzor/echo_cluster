-module(distributed_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common/include/test.hrl").

-define(APPS,      [ sasl ]).
-define(CNT_NODES, 7).
-define(TIMEOUT,   100).
-define(SLEEP_TIME, ?CNT_NODES * ?TIMEOUT).

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
order_start_test_()->
    {setup, fun setup/0, fun cleanup/1, [
        {"start cluster in order", timeout, 30, fun() ->
                Nodes = [ ec_test_tools:start_node(Id, ?CNT_NODES, ?TIMEOUT)
                          || Id <- lists:seq(1,?CNT_NODES,1) ],
                timer:sleep(?SLEEP_TIME),
                ?assertEqual(?CNT_NODES, length(Nodes)),
                ?assertEqual(?CNT_NODES, length(nodes())),

                Masters = [ec_test_tools:get_master(Node) || Node <- lists:sort(nodes())],
                ShouldBeMasterId = 7,
                ?assertEqual(6=?CNT_NODES-1, ec_test_tools:assert_master(ShouldBeMasterId, Masters)),

                distributed_node:stop_nodes(Nodes),
                ok
        end}
    ]}.

shuffle_start_test_()->
    {setup, fun setup/0, fun cleanup/1, [
        {"shuffle nodes and try to start this", timeout, 30, fun() ->
                Nodes = [ ec_test_tools:start_node(Id, ?CNT_NODES, ?TIMEOUT)
                          || Id <- ec_test_tools:shuffle_list(lists:seq(1,?CNT_NODES,1)) ],
                timer:sleep(?SLEEP_TIME),
                ?assertEqual(?CNT_NODES, length(Nodes)),
                ?assertEqual(?CNT_NODES, length(nodes())),

                Masters = [ec_test_tools:get_master(Node) || Node <- lists:sort(nodes())],
                ShouldBeMasterId = 7,
                ?assertEqual(6=?CNT_NODES-1, ec_test_tools:assert_master(ShouldBeMasterId, Masters)),

                distributed_node:stop_nodes(Nodes),
                ok
        end}
    ]}.

slave_halt_test_()->
    {setup, fun setup/0, fun cleanup/1, [
        {"halt slave and check than master not changed", timeout, 30, fun() ->
                Nodes = [ ec_test_tools:start_node(Id, ?CNT_NODES, ?TIMEOUT)
                          || Id <- ec_test_tools:shuffle_list(lists:seq(1,?CNT_NODES,1))],
                timer:sleep(?SLEEP_TIME),
                ?assertEqual(?CNT_NODES, length(Nodes)),
                ?assertEqual(?CNT_NODES, length(nodes())),

                distributed_node:stop_applications(ec_test_tools:node_name(1), [echo_cluster]),
                timer:sleep(?SLEEP_TIME),
                NewMasters = [ec_test_tools:get_master(Node) || Node <- lists:sort(nodes())],
                NewShouldBeMasterId = 7,
                ?assertEqual(5=?CNT_NODES-1-1, ec_test_tools:assert_master(NewShouldBeMasterId, NewMasters)),

                distributed_node:stop_nodes(Nodes),
                ok
        end}
    ]}.

complex_case_test_()->
    {setup, fun setup/0, fun cleanup/1, [
        {"complex test: halt 7 than 5 and 6 after it start 7", timeout, 60, fun() ->
                Nodes = [ ec_test_tools:start_node(Id, ?CNT_NODES, ?TIMEOUT)
                          || Id <- ec_test_tools:shuffle_list(lists:seq(1,?CNT_NODES,1))],
                timer:sleep(?SLEEP_TIME),
                ?assertEqual(?CNT_NODES, length(Nodes)),
                ?assertEqual(?CNT_NODES, length(nodes())),

                Masters = [ec_test_tools:get_master(Node) || Node <- lists:sort(nodes())],
                ShouldBeMasterId = 7,
                ?assertEqual(6=?CNT_NODES-1, ec_test_tools:assert_master(ShouldBeMasterId, Masters)),

                distributed_node:stop_applications(ec_test_tools:node_name(7), [echo_cluster]),
                timer:sleep(?SLEEP_TIME),
                NewMasters = [ec_test_tools:get_master(Node) || Node <- lists:sort(nodes())],
                NewShouldBeMasterId = 6,
                ?assertEqual(5=?CNT_NODES-1-1, ec_test_tools:assert_master(NewShouldBeMasterId, NewMasters)),

                distributed_node:stop_applications(ec_test_tools:node_name(5), [echo_cluster]),
                distributed_node:stop_applications(ec_test_tools:node_name(6), [echo_cluster]),
                timer:sleep(?SLEEP_TIME),
                ?assertEqual(3, ec_test_tools:assert_master(4, [ec_test_tools:get_master(Node)
                                                                || Node <- lists:sort(nodes())])),

                distributed_node:start_applications(ec_test_tools:node_name(7), [echo_cluster]),
                timer:sleep(?SLEEP_TIME),
                ?assertEqual(4, ec_test_tools:assert_master(7, [ec_test_tools:get_master(Node)
                                                                || Node <- lists:sort(nodes())])),

                distributed_node:stop_nodes(Nodes),
                ok
        end}
    ]}.

%% =============================================================================
%%  Helpers
%% =============================================================================
