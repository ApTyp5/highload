%%%-------------------------------------------------------------------
%%% @author arthur
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Oct 2020 12:21
%%%-------------------------------------------------------------------
-author("aptyp potatoch").

-export_type([classic_return/1]).

-type
	classic_return(RetVal)
		::      {ok, RetVal}
		|       {error, Reason :: any()}.
