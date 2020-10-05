%%%-------------------------------------------------------------------
%%% @author arthur
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2020 12:55
%%%-------------------------------------------------------------------
-author("arthur").

-record(server_props, {workerNum = -1 :: number(), root = <<>> :: binary(), port = 8080 :: number()}).
