-module(start_stop_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common/include/test.hrl").

-define(APPS, [ sasl ]).

%% =============================================================================
%%  Common
%% =============================================================================

setup()->
    error_logger:tty(false),
    ?START_APPS(
        ?APPS, [
            {sasl, [ {sasl_error_logger, {file, atom_to_list(?MODULE)++".log"}} ]}
            ]
        ),
    ok.

cleanup(_)->
    ?STOP_APPS(?APPS),
    error_logger:tty(true),
    ok.

%% =============================================================================
%%  Tests
%% =============================================================================
main_test_()->
    ?FIXTURE(
        fun()->
            ?assertEqual(ok, application:start(echo_cluster)),
            timer:sleep(75),
            ?assertEqual(
                {error,{already_started, echo_cluster}},
                application:start(echo_cluster)
                ),
            ?assertEqual(ok, application:stop(echo_cluster)),
            ok
        end
        ).

%% =============================================================================
%%  Helpers
%% =============================================================================
