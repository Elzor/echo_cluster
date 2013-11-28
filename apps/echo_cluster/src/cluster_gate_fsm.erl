-module(cluster_gate_fsm).

-behaviour(gen_fsm).

-author('Maxim Molchanov <elzor.job@gmail.com>').


%% API
-export([start_link/0, master/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% states
-export([idle/2, vote/2, master/2, slave/2,
         wait_for_fine/2, wait_for_king/2]).

%% includes
-include_lib("kernel/include/inet.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common/include/log.hrl").

%% records
-record(state, { nodeid        = undefined     :: integer(),
                 iammaster     = false         :: boolean(),
                 socket        = undefined     :: gen_udp:socket(),
                 nodes         = orddict:new() :: orddict:orddict(),
                 timer         = undefined     :: reference(),
                 pong_received = false         :: boolean() }).

-record(node, { ip     = {0,0,0,0} :: tuple(),
                port   = 5000      :: integer(),
                master = false     :: boolean() }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Get master
%%
%% @spec master() -> ok.
%% @end
%%--------------------------------------------------------------------
master(DestPid) ->
    erlang:send(?MODULE, {get_master, DestPid}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?DBG("start cluster node: ~p", [node()]),
    process_flag(trap_exit, true),
    {ok, Id}     = application:get_env(echo_cluster, nodeid),
    {ok, Ipaddr} = application:get_env(echo_cluster, ipaddress),
    {ok, Port}   = application:get_env(echo_cluster, port),
    {ok, Nodes}  = application:get_env(echo_cluster, cluster),
    {ok, Socket} = gen_udp:open(Port, [inet, {ip, Ipaddr}, binary]),
    gen_fsm:send_event(self(), start_voting),
    {ok, idle, #state{ nodeid = Id, socket = Socket, nodes = init_state(nodes, Nodes) }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
idle(start_voting, State) ->
    gen_fsm:send_event(self(), send_alive_to_outrank),
    {next_state, vote, State};
idle(Event, State) ->
    ?DBG("~p in idle: ~p", [node(), Event]),
    {next_state, idle, State}.

%%--------------------------------------------------------------------
vote(send_alive_to_outrank, #state{nodeid = MyId, socket = Socket, nodes = Nodes} = State) ->
    OutRankNodes = outrank_nodes(MyId, Nodes),
    ?DBG("~p on ~p to ~p", [send_alive_to_outrank, node(), OutRankNodes]),
    send_message(Socket, OutRankNodes, 'ALIVE?'),
    Timer = gen_fsm:send_event_after(interval(), change_state_to_master),
    {next_state, vote, State#state{timer = Timer}};
vote(change_state_to_master, #state{socket = Socket, nodes = Nodes} = State) ->
    ?DBG("~p on ~p", [change_state_to_master, node()]),
    send_message(Socket, Nodes, 'IAMTHEKING'),
    {next_state, master, State#state{nodes = clear_master(Nodes)}};
vote({handle_msg, FromIp, FromPort, {ok,'IAMTHEKING'}}, State) ->
    ?DBG("in vote ~p change master to ~p", [node(), FromIp]),
    new_state_when_receive('IAMTHEKING', FromIp, FromPort, State);
vote({handle_msg, _FromIp, _FromPort, {ok,'FINETHANKS'}}, #state{timer=_OldTimer, nodes=_Nodes} = State) ->
    ?DBG("in vote ~p receive ~p", [node(), fine]),
    Timer = gen_fsm:send_event_after(interval(), change_state_to_vote),
    {next_state, wait_for_king, State#state{timer = Timer}};
vote({handle_msg, FromIp, FromPort, {ok,'ALIVE?'}}, State) ->
    ?DBG("in vote ~p receive ~p", [node(), alive]),
    new_state_when_receive('ALIVE?', FromIp, FromPort, State);
vote(Event, State) ->
    ?DBG("~p in vote: ~p", [node(), Event]),
    {next_state, vote, State}.

%%--------------------------------------------------------------------
wait_for_fine(change_state_to_vote, State) ->
    ?DBG("in wait_for_fine ~p no one king exists", [node()]),
    gen_fsm:send_event(self(), send_alive_to_outrank),
    {next_state, vote, State};
wait_for_fine({handle_msg, FromIp, FromPort, {ok,'ALIVE?'}}, State) ->
    ?DBG("in wait_for_fine ~p receive ~p", [node(), alive]),
    new_state_when_receive('ALIVE?', FromIp, FromPort, State);
wait_for_fine({handle_msg, _FromIp, _FromPort, {ok,'FINETHANKS'}}, #state{nodes=_Nodes} = State) ->
    ?DBG("in wait_for_fine ~p receive ~p", [node(), fine]),
    Timer = gen_fsm:send_event_after(interval(), change_state_to_vote),
    {next_state, wait_for_king, State#state{timer = Timer}};
wait_for_fine(Event, State) ->
    ?DBG("~p in wait_for_fine: ~p", [node(), Event]),
    {next_state, wait_for_fine, State}.

%%--------------------------------------------------------------------
wait_for_king({handle_msg, FromIp, FromPort, {ok,'IAMTHEKING'}}, State) ->
    ?DBG("in wait_for_king ~p change master to ~p", [node(), FromIp]),
    new_state_when_receive('IAMTHEKING', FromIp, FromPort, State);
wait_for_king(Event, State) ->
    ?DBG("~p in wait_for_king: ~p", [node(), Event]),
    {next_state, wait_for_king, State}.

%%--------------------------------------------------------------------
master({handle_msg, FromIp, FromPort, {ok,'PING'}}, #state{socket = Socket} = State) ->
    ?DBG("in master ~p handle ping from ~p", [node(), FromIp]),
    gen_udp:send(Socket, FromIp, FromPort, encode_command('PONG')),
    {next_state, master, State};
master({handle_msg, FromIp, FromPort, {ok,'ALIVE?'}}, State) ->
    ?DBG("in master ~p receive ~p", [node(), alive]),
    new_state_when_receive('ALIVE?', FromIp, FromPort, State);
master({handle_msg, FromIp, FromPort, {ok,'IAMTHEKING'}}, State) ->
    ?DBG("in master ~p change master to ~p", [node(), FromIp]),
    new_state_when_receive('IAMTHEKING', FromIp, FromPort, State);
master(Event, State) ->
    ?DBG("~p in master: ~p", [node(), Event]),
    {next_state, master, State}.

%%--------------------------------------------------------------------
slave({ping_master, 0}, State) ->
    ?DBG("in slave ~p master not responced", [node()]),
    gen_fsm:send_event(self(), send_alive_to_outrank),
    {next_state, vote, State};
slave({ping_master, N}, #state{socket=Socket, nodes=Nodes, pong_received=PongReceived} = State) ->
    ?DBG("in slave ~p ping master", [node()]),
    case get_master(Nodes) of
        [{_Id, MasterNode}] ->
            gen_udp:send(Socket, MasterNode#node.ip, MasterNode#node.port, encode_command('PING'));
        _ -> ?ERR("why?"), pass
    end,
    NewN = case PongReceived of
        true  -> 3;
        false -> N - 1
    end,
    {next_state, slave, State#state{pong_received = false, timer=gen_fsm:send_event_after(interval(), {ping_master, NewN}) }};
slave({handle_msg, _FromIp, _FromPort, {ok,'PONG'}}, State) ->
    ?DBG("in slave ~p handle pong", [node()]),
    {next_state, slave, State#state{pong_received = true}};
slave({handle_msg, FromIp, FromPort, {ok,'IAMTHEKING'}}, #state{nodes=Nodes} = State) ->
    ?DBG("in slave ~p change master to ~p", [node(), FromIp]),
    {next_state, slave, State#state{nodes=set_master(FromIp, FromPort, Nodes)}};
slave({handle_msg, FromIp, FromPort, {ok,'ALIVE?'}}, State) ->
    ?DBG("in slave ~p receive ~p", [node(), alive]),
    new_state_when_receive('ALIVE?', FromIp, FromPort, State);
slave(Event, State) ->
    ?DBG("~p in slave: ~p", [node(), Event]),
    {next_state, slave, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(Event, StateName, State) ->
    ?DBG({Event, StateName}),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, State) ->
    ?ERR("unhandled sync_event:~p",[Event]),
    {reply, ok, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({udp, Socket, FromIp, FromPort, Packet}, StateName, #state{socket = Socket} = State) ->
    gen_fsm:send_event(self(), {handle_msg, FromIp, FromPort, decode_command(Packet)}),
    {next_state, StateName, State};

handle_info({get_master, DestPid}, StateName, #state{nodes = Nodes} = State) ->
    DestPid ! {ok, get_master(Nodes)},
    {next_state, StateName, State};

handle_info(Info, StateName, State) ->
    ?DBG("~p:~p", [node(), Info]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _StateName, #state{socket = Socket} = _State) ->
    ?DBG("terminate cluster node ~p with reason ~p", [node(), Reason]),
    gen_udp:close(Socket),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% State transformers
%%%===================================================================
new_state_when_receive('ALIVE?', FromIp, FromPort, #state{nodeid=MyId, socket=Socket, timer = OldTimer, nodes = Nodes} = State) ->
    cancel_timer(OldTimer),
    case outrank_nodes(MyId, Nodes) of
        [] ->
            ?DBG("~p on ~p", [change_state_to_master_by_alive, node()]),
            send_message(Socket, Nodes, 'IAMTHEKING'),
            {next_state, master, State#state{nodes = clear_master(Nodes)}};
        _ ->
            gen_udp:send(Socket, FromIp, FromPort, encode_command('FINETHANKS')),
            gen_fsm:send_event(self(), send_alive_to_outrank),
            {next_state, vote, State}
    end;
new_state_when_receive('IAMTHEKING', FromIp, FromPort, #state{nodes=Nodes, timer = OldTimer} = State) ->
    cancel_timer(OldTimer),
    gen_fsm:send_event(self(), {ping_master,3}),
    {next_state, slave, State#state{nodes=set_master(FromIp, FromPort, Nodes)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_state(nodes, Nodes) ->
    orddict:from_list([ {Id, #node{ip=Ip, port=Port, master=false}}
                         || {Id, Ip, Port} <- Nodes ]).

outrank_nodes(MyId, Nodes) ->
    lists:filter(fun({Id, _Node}) -> MyId < Id end, Nodes).

send_message(Socket, Nodes, Message) ->
    [ gen_udp:send(Socket, Node#node.ip, Node#node.port, encode_command(Message))
      || {_Id, Node} <- Nodes ],
    ok.

get_master(Nodes) ->
    lists:filter(fun({_Id, Node}) -> Node#node.master end, Nodes).

clear_master(Nodes) ->
    orddict:map(fun(_Id, Node) ->
        Node#node{master=false}
    end, Nodes).

set_master(Ip, Port, Nodes) ->
    NodeId   = id_by_ip(Ip, Port, Nodes),
    orddict:map(fun(Id, Node) ->
        case Id of
            NodeId -> Node#node{master=true};
            _      -> Node#node{master=false}
        end
    end, Nodes).

id_by_ip(Ip, Port, Nodes) ->
    case lists:filter(fun({_Id, Node}) ->
                    case {Node#node.ip =:= Ip, Node#node.port =:= Port} of
                        {true, true} -> true;
                        _            -> false
                    end
                 end, Nodes) of
        [{Id, _}] -> Id;
        _         -> error
    end.

interval() ->
    {ok, Interval} = application:get_env(echo_cluster, tick_interval_ms),
    Interval.

cancel_timer(Timer) when is_reference(Timer) ->
    erlang:cancel_timer(Timer);
cancel_timer(_) ->
    ok.

encode_command('ALIVE?')     -> <<1>>;
encode_command('FINETHANKS') -> <<2>>;
encode_command('IAMTHEKING') -> <<3>>;
encode_command('PING')       -> <<4>>;
encode_command('PONG')       -> <<5>>.

decode_command(<<1>>) -> {ok,  'ALIVE?'};
decode_command(<<2>>) -> {ok,  'FINETHANKS'};
decode_command(<<3>>) -> {ok,  'IAMTHEKING'};
decode_command(<<4>>) -> {ok,  'PING'};
decode_command(<<5>>) -> {ok,  'PONG'};
decode_command(_)     -> {err, 'unknown_command'}.
