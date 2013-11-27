-ifndef(TEST_HRL).
-define(TEST_HRL, true).

-include_lib("eunit/include/eunit.hrl").

-define(FIXTURE_DEFAULT_TIMEOUT, 60).
-define(FIXTURE(TestFun, Timeout), {
        setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
                {timeout, Timeout, TestFun}
        end}).

-define(FIXTURE(TestFun), ?FIXTURE(TestFun, ?FIXTURE_DEFAULT_TIMEOUT)).

%% run test fun in separate subprocess
-define(FIXTURE_SPAWN(TestFun, Timeout), {
        setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
                {spawn, {timeout, Timeout, fun() ->
                                ?LOG_TEST_STARTED,
                                TestFun(),
                                ?LOG_TEST_FINISHED
                        end}}
        end}).

-define(FIXTURE_SPAWN(TestFun), ?FIXTURE_SPAWN(TestFun, ?FIXTURE_DEFAULT_TIMEOUT)).

-define(LOG_TEST_STARTED,
    ?debugFmt("==================== Test started: ~p", [?CURRENT_FUN_NAME])).

-define(LOG_TEST_FINISHED,
    ?debugFmt("==================== Test finished: ~p", [?CURRENT_FUN_NAME])).

-define(LOG_FUN_ENTERING,
    ?debugFmt("========== Entering: ~p", [?CURRENT_FUN_NAME])).

-define(LOG_FUN_LEAVING,
    ?debugFmt("========== Leaving: ~p", [?CURRENT_FUN_NAME])).

-define(LOG_TRACE_POINT(Msg),
    ?debugFmt("---------- ~p", [Msg])).


-define(wait_for_msg_data(Timeout),
    ((fun () ->
            receive
                #'$msg' {message = Data, tag = Tag} ->
                    {ok, Data, Tag};
                _Other ->
                    ?debugFmt("~nunexpected msg: ~p~n", [_Other]),
                    ?ERR_MOD("unexpected msg: ~p", [_Other]),
                    {error, {unexpected_message, _Other}}
            after
                Timeout ->
                    {error, timeout}
            end
    end)())).

-define(SETUP_LOGGER(), begin
                            error_logger:tty(false),
                            error_logger:logfile({open, "test.log"})
                        end).
-define(CLEANUP_LOGGER(), begin
                            error_logger:tty(true)
                          end).



%% start applications for test, for using in setup/0 fun
-define(START_APPS(Apps, Params),
    begin
        ?SETUP_LOGGER(),
        lists:foreach(
            fun(App) ->
                application:stop(App),
                application:unload(App),
                ?assertEqual(ok, application:load(App)),
                AppParams = proplists:get_value(App, Params, []),
                lists:foreach(
                    fun({Key, Value}) ->
                        ?assertEqual(ok, application:set_env(App, Key, Value))
                    end,
                    AppParams
                )
            end,
            Apps
        ),
        StartRes = lists:foldl(
            fun(App, Acc) ->
                [{App, application:start(App)} | Acc]
            end,
            [], Apps
        ),
        lists:foreach(
            fun(AppStartRes) ->
                ?assertMatch({_App, ok}, AppStartRes)
            end,
            StartRes
        )
    end
).

%% stop applications for test, for using in cleanup/1 fun
-define(STOP_APPS(Apps),
    begin
        lists:foreach(
            fun(App) ->
                ?assertEqual(ok, application:stop(App)),
                ?assertEqual(ok, application:unload(App))
            end,
            lists:reverse(Apps)
        ),
        ?CLEANUP_LOGGER()
    end
).


-endif.
