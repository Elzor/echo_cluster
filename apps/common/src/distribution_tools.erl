-module(distribution_tools).

-compile([nowarn_unused_function]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common/include/log.hrl").

-export([
    construct_node_name/1,
    remote_start_node/2,
    start_node/2,
    start_node/3
    ]).
-export([poll_until/3]).

%% Construct node (atom) from name (string).
%% Host name is extracted from this node.
construct_node_name(Name) when is_list(Name) ->
    [_Name, Host] = string:tokens(atom_to_list(node()), "@"),
    NewNodeName = Name ++ "@" ++ Host,
    list_to_atom(NewNodeName).


%% Run new node with name 'NodeName' as atom 'name@host'
%% Params - proplist:
%%  {contact_node, Node}
%%  {run_function, {Module, Function}}
remote_start_node(Node, Params) ->
    %% ?TRC(""),
    Cookie            = create_cookie_string(),
    NodeString        = create_sname_string(Node),
    ContactNodeString = create_contact_node_string(Params),
    StartFunction     = create_start_function_string(Params),
    PAString          = create_pa_string(Params),
    AppParams         = create_app_params(Params),
    CmdString         = "start \""++NodeString++"\" erl -noshell" ++ NodeString ++ Cookie ++
        ContactNodeString ++ StartFunction ++ PAString ++ AppParams,
    ?LOG("CmdString = ~s",[CmdString]),
    Fun = fun() ->
            os:cmd(CmdString)
    end,
    spawn(Fun),

    ensure_ping(Node),
    ok.

start_node(Node,Params) ->
    {ok,HostName} = inet:gethostname(),
    Host          = list_to_atom(HostName),
    start_node(Host,Node,Params).

start_node(Host,Node,Params) ->
    ParamsStr = lists:flatten(
            [
                create_cookie_string(),
                create_config_string(Params),
                create_contact_node_string(Params),
                create_start_function_string(Params),
                create_pa_string(Params),
                create_app_params(Params),
                make_code_path(),
                make_rsh_config()
                ]
            ),
    StartRes = slave:start(Host,Node,ParamsStr),
    ?assertMatch({ok, _NodeName}, StartRes),
    StartRes.

make_rsh_config()->
    " -rsh ssh".

make_code_path() ->
    lists:foldl(fun(Node, Path) -> " -pa " ++ Node ++ Path end, [], non_std_paths()).
non_std_paths() ->
    lists:filter(fun(Path) -> not(lists:prefix(code:lib_dir(), Path)) end, code:get_path()).

create_config_string(Params) ->
    case proplists:get_value(config_file, Params) of
        ConfigFile when is_list(ConfigFile) ->
            case check_config_file(ConfigFile) of
                true -> " -config " ++ ConfigFile;
                _ -> ""
            end;
        _ -> ""
    end.

create_sname_string(Node) ->
    " -sname " ++ atom_to_list(Node).

create_cookie_string() ->
    " -setcookie " ++ atom_to_list(erlang:get_cookie()).

create_contact_node_string(Params) ->
    case proplists:get_value(contact_node, Params) of
        ContactNode when is_atom(ContactNode) ->
            " -contact_node " ++ atom_to_list(ContactNode);
        _ -> ""
    end.

create_start_function_string(Params) ->
    case proplists:get_value(start_function, Params) of
        {Module, Function, Arg} ->
            ArgString = create_arg_string(Arg),
            " -eval \""++ atom_to_list(Module) ++ ":" ++ atom_to_list(Function) ++ "(" ++ ArgString ++ ").\"";
        _ -> ""
    end.

create_arg_string(Arg) when is_pid(Arg) ->
    pid_to_list(Arg);
create_arg_string(Arg) when is_list(Arg) ->
    Arg.

create_pa_string(Params) ->
    case proplists:get_value(pa, Params) of
        Path when is_list(Path) -> " -pa "++ filename:nativename(Path);
        _ -> ""
    end.

create_app_params(Params) ->
    case proplists:get_value(app_params, Params) of
        AppParams when is_list(AppParams) -> process_app_params(AppParams, "");
        _ -> ""
    end.

process_app_params([], ResultString) ->
    %% ?LOG("process_app_params: ~s", [ResultString]),
    ResultString;
process_app_params([ {App, Param, Value} | TailParams], ResultString) ->
    AppString       = " -" ++ atom_to_list(App),
    ParamString     = " " ++ atom_to_list(Param),
    ValueString     = " \"" ++ lists:flatten(io_lib:format("~w",[Value])) ++ "\"",
    NewResultString = ResultString ++  AppString ++ ParamString ++ ValueString,
    process_app_params(TailParams, NewResultString).


ensure_ping(Node) ->
    PingFun = fun() ->
            case net_adm:ping(Node) of
                pong -> true;       % break pinging
                pang -> false       % continue pinging
            end
    end,
    ?assertEqual(true, poll_until(PingFun, 10, 500)).


%%--------------------------------------------------------------------
%% @doc This is a higher order function that allows for Iterations
%%      number of executions of Fun until false is not returned
%%      from the Fun pausing for PauseMS after each execution.
%% <pre>
%% Variables:
%%  Fun - A fun to execute per iteration.
%%  Iterations - The maximum number of iterations to try getting Reply out of Fun.
%%  PauseMS - The number of miliseconds to wait inbetween each iteration.
%%  Return - What ever the fun returns.
%% </pre>
%% @end
%%--------------------------------------------------------------------
-spec poll_until(term(), timeout(), timeout()) -> term() | false.
poll_until(Fun, 0, _PauseMS) ->
    Fun();
poll_until(Fun, Iterations, PauseMS) ->
    case Fun() of
        false ->
            timer:sleep(PauseMS),
            case Iterations of
                infinity   -> poll_until(Fun, Iterations, PauseMS);
                Iterations -> poll_until(Fun, Iterations - 1, PauseMS)
            end;
        Reply ->
            Reply
    end.

%% replace FromChar to ToString in Data
replace_char(FromChar, ToString, Data) ->
    replace_char_acc(FromChar, ToString, Data, []).

replace_char_acc(_, _, [], Acc) ->
    lists:reverse(Acc);
replace_char_acc(Char, String, [Head | Tail], Acc) ->
    if
        Char == Head -> replace_char_acc(Char, String, Tail, [String | Acc]);
        true -> replace_char_acc(Char, String, Tail, [Head | Acc])
    end.


check_config_file(Name) ->
    FileName = Name ++ ".config",
    case file:open(FileName, [read]) of
        {ok, FileHandle} ->
            file:close(FileHandle),
            true;
        {error,Reason} ->
            {ok, Dir} = file:get_cwd(),
            ?WRN("distribution_tools:check_config_file, error reading config file ~s, reason ~p, using defaults, wd is ~s",[FileName,Reason,Dir]),
            false
    end.
