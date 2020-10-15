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

handleAccepts(AcceptorProps) when is_record(AcceptorProps, acceptor_props)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, AcceptorProps]),
	handleAcceptsCounter(AcceptorProps, 1).

handleAcceptsCounter(#acceptor_props{listenSock = LSock, root = Root}, Counter)
	-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [LSock, Root]]),
	case gen_tcp:accept(LSock) of
		{ok, Sock} ->
			?LOG_NOTICE("handle ~p connection on socket ~p~n", [Counter, Sock]),
			fastHandleConnection(Sock, Root),
			?LOG_NOTICE("connection ~p on socket ~p handled~n", [Counter, Sock]);
		{error, Reason} ->
			?LOG_ERROR("accept error: ~p~n", [Reason])
	end,
	?LOG_DEBUG("endHandleAccepts"),
	handleAcceptsCounter(#acceptor_props{listenSock = LSock, root = Root}, Counter + 1).

fastHandleConnection(Socket, Root)
		when is_port(Socket) and is_binary(Root)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [Socket, Root]]),
	ok = handleRecv(gen_tcp:recv(Socket, 0), Socket, Root),
	gen_tcp:close(Socket),
	?LOG_DEBUG("connection closed"),
	ok.


-spec handleConnection(Socket ::port(), Root :: binary()) -> ok.
handleConnection(Socket, Root)
		when is_port(Socket) and is_binary(Root)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [Socket, Root]]),
	receive
		handle ->
			ok = handleRecv(gen_tcp:recv(Socket, 0), Socket, Root),
			gen_tcp:close(Socket),
			?LOG_DEBUG("connection closed")
	end,
	?LOG_DEBUG("end worker"),
	ok.


handleRecv({ok, Packet}, Socket, Root)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, Packet]),
	case prepareAnswer(Packet, Root) of
		{stop, HttpRespHeadersBin} ->
			gen_tcp:send(Socket, HttpRespHeadersBin);
		{stop, HttpRespHeadersBin, _} ->
			gen_tcp:send(Socket, HttpRespHeadersBin);
		{cont, HttpRespHeadersBin, AbsPath} ->
			?LOG_DEBUG("answer prep-d, start send it"),
			gen_tcp:send(Socket, HttpRespHeadersBin),
			?LOG_DEBUG("answer sent, start send file"),
			file:sendfile(AbsPath, Socket),
			?LOG_DEBUG("file sent")
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
	Out = case headersResponse(HttpReq, AbsPath) of
		{stop, HeadersRespBin} -> {stop, HeadersRespBin};
		{ok, HeadersRespBin} -> {cont, HeadersRespBin}
	end,
	?LOG_DEBUG("prep headers done"),
	Out;

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
