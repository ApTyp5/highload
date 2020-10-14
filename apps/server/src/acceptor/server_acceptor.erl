%%%-------------------------------------------------------------------
%%% @author arthur
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Oct 2020 11:36
%%%-------------------------------------------------------------------
-module(server_acceptor).
-author("arthur").
-include("./acceptor_props.hrl").
-include("../../../simple_http/src/types.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

%% behavior
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start_link/1, handleConnection/2]).

init(Props) when is_record(Props, acceptor_props)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, Props]),
	{ok, Props}.

handle_call(_Request, _From, _State) ->
	erlang:error(not_implemented).

handle_cast(start_accept, Props) when is_record(Props, acceptor_props) ->
	handleAccepts(Props).

%% API

start_link(Props) when is_record(Props, acceptor_props) -> ?LOG_NOTICE("acceptor started with props: ~p~n", [Props]),
	gen_server:start_link(?MODULE, Props, []).

handleAccepts(#acceptor_props{listenSock = LSock, root = Root})
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [LSock, Root]]),
	case gen_tcp:accept(LSock) of
		{ok, Sock} ->
			Pid = spawn(?MODULE, handleConnection, [Sock, Root]),
			ok = handleControllingProcess(gen_tcp:controlling_process(Sock, Pid), Sock, Pid),
			Pid ! handle;
		{error, Reason} ->
			?LOG_ERROR("accept error: ~p~n", [Reason])
	end,
	handleAccepts(#acceptor_props{listenSock = LSock, root = Root}).


handleControllingProcess(ok, _Sock, _Pid) ->
%%	-> ?LOG_NOTICE("controlling process success: ~p~n", [{Sock, Pid}]),
	ok;
handleControllingProcess({error, Reason}, Sock, Pid)
	-> ?LOG_NOTICE("controlling process trouble: ~p~n", [{Reason, Sock, Pid}]),
	ok;
handleControllingProcess(Smth, Sock, Pid)
	-> ?LOG_ERROR("controlling process error: ~p~n", [{Smth, Sock, Pid}]),
	ok.



-spec handleConnection(Socket ::port(), Root :: binary()) -> ok.
handleConnection(Socket, Root)
		when is_port(Socket) and is_binary(Root)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [Socket, Root]]),
	receive
		handle ->
			ok = handleRecv(gen_tcp:recv(Socket, 0, 500), Socket, Root),
			gen_tcp:close(Socket)
	end,
	ok.


handleRecv({ok, Packet}, Socket, Root)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, Packet]),
	case prepareAnswer(Packet, Root) of
		{stop, HttpRespHeadersBin} ->
			gen_tcp:send(Socket, HttpRespHeadersBin);
		{stop, HttpRespHeadersBin, _} ->
			gen_tcp:send(Socket, HttpRespHeadersBin);
		{cont, HttpRespHeadersBin, AbsPath} ->
			gen_tcp:send(Socket, HttpRespHeadersBin),
			file:sendfile(AbsPath, Socket)
	end,
	ok;
handleRecv(Error, _, _)
		-> ?LOG_NOTICE("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, Error]),
	ok.

prepareAnswer(Packet, Root)
		when is_binary(Packet) and is_binary(Root)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [Packet, Root]]),
	case simple_http:parse(Packet) of
		{error, not_enough_lines} ->
			{stop, <<>>};
		{error, bad_request} ->
			{stop, badRequestResponse()};
		{ok, HttpReq} when is_record(HttpReq, http_request) ->
			AbsPath = <<Root/binary, (HttpReq#http_request.requestURI)/binary>>,
			{Action, HttpRespHeadersBin} = prepareResponseHeaders(HttpReq#http_request.method, HttpReq, AbsPath),
			{Action, HttpRespHeadersBin, AbsPath}
	end.


prepareResponseHeaders(<<"GET">>, HttpReq, AbsPath)
		when is_record(HttpReq, http_request)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, ["GET", HttpReq, AbsPath]]),
	case headersResponse(HttpReq, AbsPath) of
		{stop, HeadersRespBin} -> {stop, HeadersRespBin};
		{ok, HeadersRespBin} -> {cont, HeadersRespBin}
	end;

prepareResponseHeaders(<<"HEAD">>, HttpReq, AbsPath)
		when is_record(HttpReq, http_request)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, ["HEAD", HttpReq, AbsPath]]),
	{_Any, HeadersRespBin} = headersResponse(HttpReq, AbsPath),
	{stop, HeadersRespBin};

prepareResponseHeaders(<<"POST">>, HttpReq, _)
		when is_record(HttpReq, http_request)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, ["POST", HttpReq]]),
	{stop, badRequestResponse()};

prepareResponseHeaders(Meth, Any, Smth)
		-> ?LOG_ERROR("unexpected ~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [Meth, Any, Smth]]).


headersResponse(HttpReq, AbsPath)
		when is_record(HttpReq, http_request) and is_binary(AbsPath)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [HttpReq, AbsPath]]),
	case file:read_file_info(AbsPath) of
		{error, enoent} ->
			{stop, notFoundResponse()};
		{error, enotdir} ->
			{stop, notFoundResponse()};
		{ok, #file_info{size = Size}} ->
			Type = extractContentTypeHeader(HttpReq#http_request.headers),
			{ok, okResponse(Size, Type)};
		{error, Reason} ->
			?LOG_ERROR("~s: read file info error: ~p~n", [?MODULE_STRING, Reason]),
			{stop, badRequestResponse()}
	end.

extractContentTypeHeader([]) -> "no_content_type";
extractContentTypeHeader([#header{key = "Content-Type", value = Type} | _Others]) -> Type;
extractContentTypeHeader([_Any | Headers]) -> extractContentTypeHeader(Headers).


okResponse(Size, Type)
		when Size >= 0
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [Size, Type]]),
	simple_http:responseToBinary(
		simple_http:generate(
			#response_version{minor = 1, major = 1},
			200,
			"OK",
			Type,
			Size
		)
	).



badRequestResponse()
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, []]),
	simple_http:responseToBinary(
		simple_http:generate(
			#response_version{major = 1, minor = 1},
			400,
			"Bad request"
		)
	).

notFoundResponse()
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, []]),
	simple_http:responseToBinary(
		simple_http:generate(
			#response_version{major = 1, minor = 1},
			404,
			"Not found"
		)
	).
