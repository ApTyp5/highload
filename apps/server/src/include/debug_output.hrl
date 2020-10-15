-author("arthur").

%% includes
-include_lib("kernel/include/logger.hrl").

-define(DEBUG_ENTRY_LOG(Args), ?LOG_DEBUG("~p:~p(~p)~n", [?MODULE_STRING, ?FUNCTION_NAME, Args])).
-define(DEBUG_EXIT_LOG(Args, Res), ?LOG_DEBUG("~p:~p(~p) -> ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, Args, Res])).
-define(DEBUG_EXIT_LOG_N_RETURN(Args, Res), ?DEBUG_EXIT_LOG(Args, Res), Res).
-define(HANDLE_DEBUG_LOG_N_RETURN(Args), ?LOG_DEBUG("~p:~p(~p)~n", [?MODULE_STRING, ?FUNCTION_NAME, Args]), Args).
-define(HANDLE_ERROR_LOG_N_RETURN(Args), ?LOG_ERROR("~p:~p(~p)~n", [?MODULE_STRING, ?FUNCTION_NAME, Args]), Args).
-define(HANDLE_ERROR_LOG_N_RETURN(Args, Res), ?LOG_ERROR("~p:~p(~p) -> ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, Args]), Args, Res).