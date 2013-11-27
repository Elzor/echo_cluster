-module(distributed_node).

-compile([nowarn_unused_function]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("common/include/log.hrl").

-behaviour(gen_server).
-export([
    start_node/1,
    start_node/2,
    stop_node/1,
    stop_nodes/1,
    start_applications/2,
    stop_applications/2,
    call_function/3,
    cast_function/3,
    wait_for_close_node/2,
    wait_for_close_nodes/2
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_process/1, ttl/0]).

-define(SERVER_NAME, ?MODULE).
-define(LOCAL_LOG, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server Side
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(NODE_TIMEOUT, 15000).

ttl()->
    file:write_file("baner", atom_to_binary(node(), utf8)).

%% Works in callee process
start_node(NodeName) ->
    start_node(NodeName, []).

start_node(NodeName, UserParams) ->
    WaitTimeout = proplists:get_value(node_timeout, UserParams, 15000),
    LocalLogStr = case proplists:get_value(local_log, UserParams, true) of
        true ->
            "true";
        _Other ->
            "false"
    end,
    % Construct param's list
    StartFunParams = {start_function,
            {?MODULE, start_process, "[{wait," ++ timeout_to_string(WaitTimeout)++"},{local_log," ++ LocalLogStr ++ "}]"}
            },
    Params = case proplists:get_value(contact_node, UserParams) of
        undefined ->
            [ {contact_node, node()}, StartFunParams | UserParams];
        _ ->
            [ StartFunParams | UserParams ]
    end,
    %% Start node
    Node = distribution_tools:construct_node_name(NodeName),
    distribution_tools:start_node(list_to_atom(NodeName), Params),
    distribution_tools:poll_until(fun() ->
                pong == (catch gen_server:call({?SERVER_NAME, Node}, ping))
        end,
        10, 500 ),
    Node.

timeout_to_string(infinity) ->
    atom_to_list(infinity);
timeout_to_string(Timeout) when is_integer(Timeout) ->
    integer_to_list(Timeout).

%% Stop specified node
stop_node(Node) ->
    gen_server:cast({?SERVER_NAME,Node}, stop).

stop_nodes(Nodes) when is_list(Nodes) ->
    [ stop_node(Node) || Node <- Nodes].

%% Start application list on specified node
start_applications(Node, AppList) ->
    ?DBG_MOD("start_applications(): Node= ~p, AppList = ~p", [Node, AppList]),
    gen_server:call({?SERVER_NAME,Node}, {start_apps, AppList}, 60000).

%% Stop application list on specified node
stop_applications(Node, AppList) ->
    ?DBG_MOD("stop_applications(): Node= ~p, AppList = ~p", [Node, AppList]),
    gen_server:call({?SERVER_NAME,Node}, {stop_apps, AppList}, 60000).

%% Call function on specified node
%% Module name is needed for correct call
call_function(Node, Module, Function) when is_function(Function) ->
    ?LOG("call_function(): Node= ~p, Module = ~p, Function = ~p", [Node, Module, Function]),
    Path = filename:dirname(code:which(Module)),
    gen_server:call({?SERVER_NAME,Node}, {call_function, Path, Function}, 60000).

%% Cast function on specified node
%% Module name is needed for correct call
cast_function(Node, Module, Function) when is_function(Function) ->
    ?LOG("call_function(): Node= ~p, Function = ~p", [Node, Function]),
    Path = filename:dirname(code:which(Module)),
    gen_server:cast({?SERVER_NAME,Node}, {cast_function, Path, Function}).

%% Wait for node goes out from nodes() list
wait_for_close_node(Node, Timeout) when is_atom(Node) ->
    Wait = 100,
    N = Timeout/Wait,
    wait_for_close_node(Node, Wait, N).

wait_for_close_node(_Node, _Wait, 0) ->
    timeout;
wait_for_close_node(Node, Wait, N) ->
    Nodes = nodes(),
    NodePresents = lists:member(Node, Nodes),
    case NodePresents of
        true ->
            timer:sleep(Wait),
            wait_for_close_node(Node, Wait, N-1);
        false ->
            ok
    end.

wait_for_close_nodes(Nodes, EachTimeout) when is_list(Nodes) ->
    [ wait_for_close_node(Node, EachTimeout) || Node <- Nodes].



%% =============================================================================
%% Server Side
%% =============================================================================


%% Works in remote process
start_process(Params) ->    %{wait, Timeout} = Arg]=
    StartResult = gen_server:start({local,?SERVER_NAME}, ?MODULE, Params, []),
    ?LOG("Start result: ~p", [StartResult]),
    ?LOG("Arg: ~p", [Params]),
    ?LOG("Cookie: ~p", [erlang:get_cookie()]),
    ok.

try_load_remote_code(Pid,Module) ->
    if node() =:= node(Pid) ->
            ok;  %% not need to load
        true ->
            case rpc:call(node(Pid),code,get_object_code,[Module]) of
                {Module, Binary, Filename} ->
                    case code:load_binary(Module,Filename,Binary) of
                        {module,Module} -> ok;
                        _ -> error
                    end;
                error -> error
            end
    end.

%%=================================================================================
init(Params) ->
    Timeout  = proplists:get_value(wait, Params, ?NODE_TIMEOUT),
    LocalLog = proplists:get_value(local_log, Params, ?LOCAL_LOG),
    GroupLeaderPid = whereis(user),
    group_leader(GroupLeaderPid, self()),
    error_logger:tty(false),
    LogPath = tools:logs_folder() ++ "/",
    LogName = atom_to_list(node()) ++ ".log",
    application:load(sasl),
    pg2:start(),
    if
        LocalLog ->
            % Redirect error_logger into a file
            LogFile = LogPath ++ LogName,
            error_logger:logfile({open, LogFile}),
            application:set_env(sasl, sasl_error_logger, {file, LogPath ++ "sasl_" ++ LogName});
        true ->
            application:set_env(sasl, sasl_error_logger, false),
            ok
    end,
    error_logger:info_report([
            {application, tools},
            {module, ?MODULE},
            {function, init},
            {node, node()},
            {timeout, Timeout},
            "Node has started"
            ]),
    application:start(sasl),
    {ok, {wait,Timeout}, Timeout}.

handle_call(ping, _From, {wait, Time}=State) ->
    {reply,pong, State, Time};

handle_call({call_function, Path, Function}, _From, {wait, Time}=State) ->
    error_logger:info_report([{app, ?MODULE}, {path, Path}, {cast, call_function}, {node, node()}, {"Function", Function}]),
    code:add_pathsa([Path]),
    Res = Function(),
    {reply, Res, State, Time};

handle_call({start_apps, AppList}, _From, {wait, Time}=State) ->
    %% Collect results of applications start
    StartResults = [ Res || App <- AppList, (Res = application:start(App)) /= ok],
    Res = case StartResults of
        [] -> ok;
        NotEmptyList -> NotEmptyList
    end,
    error_logger:info_report([{app, ?MODULE}, {call, start_apps}, {node, node()}, {"AppList", AppList}, {start_results, StartResults}]),
    {reply, Res, State, Time};

handle_call({stop_apps, AppList}, _From, {wait, Time}=State) ->
    error_logger:info_report([{app, ?MODULE}, {call, stop_apps}, {node, node()}, {"AppList", AppList}]),
    [ application:stop(App) || App <- AppList],
    {reply, ok, State, Time}.

handle_cast({cast_function, Path, Function}, {wait, Time}=State) ->
    ok = code:add_pathsa([Path]),
    try
        % {pid,Pid}       = erlang:fun_info(Function,pid),
        % {module,Module} = erlang:fun_info(Function,module),
        % try_load_remote_code(Pid,Module),
        Function()
    catch E:M -> ?debugVal({E,M})
    end,
    {noreply, State, Time};

handle_cast(stop, State) ->
    error_logger:info_report([{app, ?MODULE}, {cast, stop}, {node, node()}, {process, self()}]),
    {stop, normal, State}.


handle_info(timeout, State) ->
    ?LOG("Node close by timeout: ~p", [node()]),
    {stop,normal,State}.

terminate(_Reason, _State) ->
    ?LOG("Node '~p' terminated: Reason = ~p", [node(), _Reason]),
    init:stop(),                        % Close node on time out
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% =============================================================================

-spec make_distrib(
        NodeName::string()| atom(), NodeType::shortnames | longnames
        ) -> {ok, ActualNodeName::atom} | {error, Reason::term()}.
make_distrib(NodeName, NodeType) when is_list(NodeName) ->
    make_distrib(erlang:list_to_atom(NodeName), NodeType);
make_distrib(NodeName, NodeType) ->
    case node() of
        'nonode@nohost' ->
            [] = os:cmd("epmd -daemon"),
            case net_kernel:start([NodeName, NodeType]) of
                {ok, _Pid} -> node()
            end;
        CurrNode -> CurrNode
    end.
