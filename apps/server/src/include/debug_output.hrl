-author("aptyp potatoch").

%% includes
-include_lib("kernel/include/logger.hrl").

-define(DEBUG_ENTRY_LOG(Args), ?LOG_DEBUG("~p:~p(~p)~n", [?MODULE_STRING, ?FUNCTION_NAME, Args])).
-define(DEBUG_EXIT_LOG(Args, Res), ?LOG_DEBUG("~p:~p(~p) -> ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, Args, Res])).
-define(DEBUG_EXIT_LOG_N_RETURN(Args, Res), case ?DEBUG_EXIT_LOG(Args, Res) of _ -> Res end).
-define(HANDLE_DEBUG_LOG_N_RETURN(Args), case ?LOG_DEBUG("~p:~p(~p)~n", [?MODULE_STRING, ?FUNCTION_NAME, Args]) of _ -> Args end).
-define(HANDLE_DEBUG_LOG_N_RETURN(Args, Res), case ?LOG_DEBUG("~p:~p(~p)~n", [?MODULE_STRING, ?FUNCTION_NAME, Args]) of _ -> Res end).
-define(HANDLE_ERROR_LOG_N_RETURN(Args), case ?LOG_ERROR("~p:~p(~p)~n", [?MODULE_STRING, ?FUNCTION_NAME, Args]) of _ -> Args end).
-define(HANDLE_ERROR_LOG_N_RETURN(Args, Res), case ?LOG_ERROR("~p:~p(~p) -> ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, Args, Res]) of _ -> Res end).