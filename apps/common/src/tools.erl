-module(tools).

-export([i2s/2, bjoin/1, garbage_collect_all/0, get_env/2, set_env/3, get_app_env/3, module_path/1, logs_folder/0]).
-export([random_string/2, seconds_stamp/0, start_apps/1, stop_apps/1, start_all_apps/0, stop_all_apps/0]).
-export([list_to_term/1, term_to_list/1, get_aws_user_data/0]).
-export([full_node_name/1, host_name/0, random_int/2, start_sasl/0, stop_sasl/0, delete_ets_files/0]).
-export([ip4addr/0]).
-export([make/1]).
-export([make_distrib/2, stop_distrib/0]).

-include_lib("common/include/common_types.hrl").
-include_lib("common/include/log.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ALL_APPS, [
        sasl,
        mnesia,
        ibrowse
        ]).



%% i2s - formats integer Value as string, with leading zeroes, according to Width
i2s(Value,Width) ->
    i2s_acc(Value,Width,[]).
i2s_acc(Value,Width,[]) ->
    [StrValue|_] = io_lib:format("~.B",[Value]),
    i2s_acc(Value,Width,StrValue);
i2s_acc(_Value,Width,Result) when length(Result)<Width ->
    i2s_acc(_Value, Width, "0" ++ Result);
i2s_acc(_Value,_Width,Result) ->
    Result.


%% bjoin - joins lists of binaries into binary
bjoin(Binaries) ->
    lists:foldl(fun(A,Acc) ->
                <<Acc/binary, A/binary>>
        end,
        <<>>,
        Binaries).


%% garbage_collect on every process
garbage_collect_all() ->
    lists:foreach(fun(Elem) ->
                garbage_collect(Elem)
        end,
        processes()).

%% get env value
get_app_env(Application, Key, Default) ->
    case application:get_env(Application, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.

%% get env value
get_env(Application, Key) ->
    case application:get_env(Application, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            this_value_was_undefined
    end.

%% set env value
set_env(Application, Key, Value) ->
    case Value of
        this_value_was_undefined ->
            application:unset_env(Application, Key);
        _Other ->
            application:set_env(Application, Key, Value)
    end.

module_path(Module) ->
    filename:dirname(code:which(Module)).

logs_folder() ->
    filename:dirname(code:which(?MODULE)) ++ "/../../logs".


random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                [lists:nth(random:uniform(length(AllowedChars)),
                        AllowedChars)]
                ++ Acc
        end, [], lists:seq(1, Length)).


seconds_stamp() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    lists:flatten([i2s(MegaSecs,6), i2s(Secs,6), i2s(MicroSecs,6)]).

list_to_term(Data) ->
    {ok,Tokens,_} = erl_scan:string(Data),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.

term_to_list(Data) ->
    lists:flatten(io_lib:format("~p",[Data])).


start_all_apps() ->
    start_apps(?ALL_APPS).

stop_all_apps() ->
    stop_apps(?ALL_APPS).


start_apps(AppList) ->
    application:set_env(resource_discovery, contact_nodes, [node()]),
    lists:foreach(fun(App) ->
                application:start(App)
        end, AppList).

stop_apps(AppList) ->
    lists:foreach(fun(App) ->
                application:stop(App)
        end, AppList).

%%% retrieve user data, for Amazon instances
get_aws_user_data() ->
    http_client:start(),
    Url = "http://169.254.169.254/latest/user-data",
    Timeout = 3000,
    case http_client:http_request(Url, Timeout) of
        {ok, "200", _, Response} ->
            {ok, binary_to_list(Response)};
        {error, _Reason} = Error ->
            Error
    end.


%% returns full node name for local host as atom Node@host

full_node_name(Node) ->
    list_to_atom(Node ++ "@" ++ host_name()).


%% returns host name as string
host_name() ->
    StrNode = atom_to_list(node()),
    [_Node, Host | _] = string:tokens(StrNode, "@"),
    Host.

% Random number without random:seed()
random_int(Min, Max) ->
    {_, _, Now} = now(),
    round(Now/1000) rem Max + Min.

%% Start sasl and suppress its output, write logs to file
start_sasl() ->
    application:load(sasl),
    application:set_env(sasl, errlog_type, error),
    application:set_env(sasl, sasl_error_logger, {file, "sasl.log"}),
    application:start(sasl).

%% Stop sasl for symmetry
stop_sasl() ->
    application:stop(sasl),
    application:unload(sasl).

delete_ets_files() ->
    {ok, FileNames} = file:list_dir("."),
    lists:foreach(fun(FileName) ->
                case lists:reverse(FileName) of
                    [$s,$t,$e,$. | _] ->
                        file:delete(FileName);
                    _ ->
                        ok
                end
        end, FileNames).

ip4addr() ->
    {ok, AddrInfo} = inet:getifaddrs(),
    lists:foldl(fun
            ({"lo", _Params}, []) ->     % skip lo interface
                [];
            ({_ethx, Params}, []) ->     % use first available non-local interface
                case proplists:lookup(addr, Params) of
                    {addr, {_,_,_,_} = IPv4} ->
                        inet_parse:ntoa(IPv4);
                    _Other ->
                        []
                end
        end, [], AddrInfo).

make(LibName) ->
    SrcDir  = code:lib_dir(LibName, src)++"/",
    EbinDir = code:lib_dir(LibName, ebin),
    {ok, Sources} = file:list_dir(SrcDir),
    Options = [{outdir, EbinDir}],
    Files = [ SrcDir ++ File || File <- Sources],
    make:files(lists:filter(
                 fun(F)->
                         [$c, $r, $s, $.] =/= lists:sublist(lists:reverse(F), 4)
                 end, Files), Options),
    ok.


-spec make_distrib( NodeName::string()|atom(), NodeType::shortnames | longnames) ->
   {ok, ActualNodeName::atom} | {error, Reason::term()}.
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

stop_distrib()->
        net_kernel:stop().
