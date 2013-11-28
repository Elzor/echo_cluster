-module(ec_test_tools).

-include_lib("eunit/include/eunit.hrl").

-export([start_node/3, app_params/3, cluster_nodes/2, shuffle_list/1,
         get_master/1, node_name/1, assert_master/2]).

start_node(Id, CntNodes, Timeout) ->
    Node = distributed_node:start_node("node"++integer_to_list(Id),
                [app_params(Id, CntNodes, Timeout), {local_log, true}, {node_timeout, 60000}]
    ),
    ?assertEqual(ok, distributed_node:start_applications(Node, [echo_cluster])),
    Node.

app_params(NodeOrderId, CntNodes, Timeout) ->
    { app_params,
        [
            {sasl, sasl_error_logger, {file, "test_node_"++ integer_to_list(NodeOrderId) ++".log"}},
            {echo_cluster, nodeid, NodeOrderId},
            {echo_cluster, ipaddress, {127,0,0,NodeOrderId}},
            {echo_cluster, port, 5000},
            {echo_cluster, cluster, cluster_nodes(NodeOrderId, CntNodes)},
            {echo_cluster, tick_interval_ms, Timeout}
        ]
    }.

cluster_nodes(NodeOrderId, CntNodes) ->
    lists:filter(
        fun({Id, _Ip, _Port}) ->
            Id /= NodeOrderId
        end,
        [ {Id, {127,0,0,Id}, 5000} || Id <- lists:seq(1,CntNodes,1) ]
    ).

shuffle_list(List) ->
    [ X || {_,X} <- lists:sort([{random:uniform(), N} || N <- List]) ].

get_master(Node) ->
    spawn(Node, cluster_gate_fsm, master, [self()]),
    receive
        {ok, Res} -> Res;
        _Else     -> smth_else
    after 1000    -> timeout
    end.

node_name(Id) ->
    {ok, Host} = inet:gethostname(),
    list_to_atom(
        lists:flatten(["node", integer_to_list(Id), "@", Host])
    ).

assert_master(ShouldBeMasterId, Masters) ->
    lists:sum(
        lists:map(
                    fun
                        (timeout)   -> halted_node, 0;
                        ([])        -> master_node, 0;
                        ([{Id, _}]) -> ?assertEqual(ShouldBeMasterId, Id), 1
                    end,
                    Masters
        )
    ).
