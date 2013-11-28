-ifndef(LOG_HRL).
-define(LOG_HRL, true).

% -define(debug, true). % Do not uncomment this line in production!!! Uncomment only for local debugging.

-ifdef(debug).

-include_lib("eunit/include/eunit.hrl").

-define(TRC(Format),          error_logger:info_report(string:substr(?FILE,string:rchr(?FILE, $/) + 1) ++ ":~p ~s",[?LINE,Format])).
-define(DBG(Format),          ?debugVal(Format)).
-define(DBG(Format,Data),     ?debugFmt(Format,Data)).
-define(DBG_MOD(Format),      error_logger:info_msg("DBG:~p:~p, " ++ Format, [?MODULE, ?LINE])).
-define(DBG_MOD(Format, Data),error_logger:info_msg("DBG:~p:~p, " ++ Format, [?MODULE, ?LINE | Data])).
-define(DBG_RPT(Report),      error_logger:info_report(Report)).
-define(DBG_RPT(Type,Report), error_logger:info_report(Type,Report)).

-else.

-define(TRC(Format),          ok).
-define(DBG(Format),          fun(_)  -> ok end(Format)).
-define(DBG(Format,Data),     fun(_,_)-> ok end(Format,Data)).
-define(DBG_MOD(Format),      ok).
-define(DBG_MOD(Format,Data), ok).
-define(DBG_RPT(Report),      ok).
-define(DBG_RPT(Type,Report), ok).

-endif.

-define(LOG(Format),          error_logger:info_msg("~p:~p, " ++ Format, [?MODULE, ?LINE])).
-define(LOG(Format, Data),    error_logger:info_msg("~p:~p, " ++ Format, [?MODULE, ?LINE | Data])).
-define(LOG_MOD(Format),      error_logger:info_msg("~p:~p, " ++ Format, [?MODULE, ?LINE])).
-define(LOG_MOD(Format, Data),error_logger:info_msg("~p:~p, " ++ Format, [?MODULE, ?LINE | Data])).
-define(LOG_RPT(Report),      error_logger:info_report(Report)).
-define(LOG_RPT(Type,Report), error_logger:info_report(Type,Report)).
-define(LOG_FUN(Format, Data),error_logger:info_msg("~p:~p:~p, " ++ Format, [?MODULE, ?CURRENT_FUN_NAME_STR, ?LINE | Data])).
-define(LOG_FUN(Format),      error_logger:info_msg("~p:~p:~p, " ++ Format, [?MODULE, ?CURRENT_FUN_NAME_STR, ?LINE])).

-define(WRN(Format),          error_logger:warning_msg(Format)).
-define(WRN(Format, Data),    error_logger:warning_msg(Format,Data)).
-define(WRN_RPT(Report),      error_logger:warning_report(Report)).
-define(WRN_RPT(Type,Report), error_logger:warning_report(Type,Report)).
-define(ERR(Format),          error_logger:error_msg(Format)).
-define(ERR(Format, Data),    error_logger:error_msg(Format,Data)).
-define(ERR_MOD(Format),      error_logger:error_msg("~p:~p, " ++ Format, [?MODULE, ?LINE])).
-define(ERR_MOD(Format, Data),error_logger:error_msg("~p:~p, " ++ Format, [?MODULE, ?LINE | Data])).
-define(ERR_RPT(Report),      error_logger:error_report(Report)).
-define(ERR_RPT(Type,Report), error_logger:error_report(Type,Report)).

-define(CURRENT_FUN_NAME, element(2, element(2, process_info(self(), current_function)))).
-define(CURRENT_FUN_NAME_STR, atom_to_list(?CURRENT_FUN_NAME)).

-endif.
