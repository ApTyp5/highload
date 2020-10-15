-author("arthur").

%% includes
-include_lib("kernel/include/logger.hrl").

-define(DEBUG_ENTRY_LOG(Args), ?LOG_DEBUG("~p:~p(~p)~n", [?MODULE_STRING, ?FUNCTION_NAME, Args])).
-define(DEBUG_EXIT_LOG(Args, Res), ?LOG_DEBUG("~p:~p(~p) -> ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, Args, Res])).