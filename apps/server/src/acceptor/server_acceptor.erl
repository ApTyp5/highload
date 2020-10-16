-module(server_acceptor).
-author("arthur").

%% includes
-include("./acceptor_props.hrl").
-include("../../../simple_http/src/types.hrl").
-include("../include/debug_output.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

%% exports
-export([start_link/1]).

%% behavior
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

init(Props) when is_record(Props, acceptor_props) ->
	?DEBUG_ENTRY_LOG(Props),
	?DEBUG_EXIT_LOG_N_RETURN(Props, {ok, Props}).

handle_call(Request, From, State) -> ?DEBUG_EXIT_LOG_N_RETURN([Request, From, State], {error, "Not implemented"}).

handle_cast(start_accept, Props) when is_record(Props, acceptor_props) ->
	?DEBUG_ENTRY_LOG([start_accept, Props]),
	handleAccepts(Props).

%% internal functions

start_link(Props) when is_record(Props, acceptor_props) -> ?LOG_NOTICE("acceptor started with props: ~p~n", [Props]),
	gen_server:start_link(?MODULE, Props, []).


handleAccepts(#acceptor_props{listenSock = LSock, root = Root}) ->
	?DEBUG_ENTRY_LOG({LSock, Root}),
	case gen_tcp:accept(LSock) of
		{ok, Sock} -> handleConnection(Sock, Root);
		{error, Reason} -> ?LOG_ERROR("accept error: ~p~n", [Reason])
	end,
	handleAccepts(#acceptor_props{listenSock = LSock, root = Root}).


handleConnection(Socket, Root) when is_port(Socket) and is_binary(Root) ->
	?DEBUG_ENTRY_LOG([Socket, Root]),
	ok = handleRecv(gen_tcp:recv(Socket, 0), Socket, Root),
	gen_tcp:close(Socket),
	?DEBUG_EXIT_LOG_N_RETURN([Socket, Root], ok).



handleRecv({ok, Packet}, Socket, Root) ->
	?DEBUG_ENTRY_LOG([{ok, Packet}, Socket, Root]),
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
	?DEBUG_EXIT_LOG_N_RETURN([{ok, Packet}, Socket, Root], ok);

handleRecv(Error, _, _) -> ?HANDLE_ERROR_LOG_N_RETURN(Error, ok).


prepareAnswer(Packet, Root) when is_binary(Packet) and is_binary(Root) ->
	?DEBUG_ENTRY_LOG([Packet, Root]),
	Res = case simple_http:parse(Packet) of
		{error, not_enough_lines} ->
			{stop, <<>>};
		{error, bad_request} ->
			{stop, badRequestResponse()};
		{ok, HttpReq} when is_record(HttpReq, http_request) ->
			AbsPath = <<Root/binary, (HttpReq#http_request.requestURI)/binary>>,
			{Action, HttpRespHeadersBin} = prepareResponseHeaders(HttpReq#http_request.method, HttpReq, AbsPath),
			{Action, HttpRespHeadersBin, AbsPath}
	end,
	?DEBUG_EXIT_LOG_N_RETURN([Packet, Root], Res).


prepareResponseHeaders(<<"GET">>, HttpReq, AbsPath) when is_record(HttpReq, http_request) ->
	?DEBUG_ENTRY_LOG([<<"GET">>, HttpReq, AbsPath]),
	Res = case headersResponse(HttpReq, AbsPath) of
		{stop, HeadersRespBin} -> {stop, HeadersRespBin};
		{ok, HeadersRespBin} -> {cont, HeadersRespBin}
	end,
	?DEBUG_EXIT_LOG_N_RETURN([<<"GET">>, HttpReq, AbsPath], Res);

prepareResponseHeaders(<<"HEAD">>, HttpReq, AbsPath) when is_record(HttpReq, http_request) ->
	?DEBUG_ENTRY_LOG([<<"HEAD">>, HttpReq, AbsPath]),
	{_Any, HeadersRespBin} = headersResponse(HttpReq, AbsPath),
	Res = {stop, HeadersRespBin},
	?DEBUG_EXIT_LOG_N_RETURN([<<"HEAD">>, HttpReq, AbsPath], Res);

prepareResponseHeaders(<<"POST">>, HttpReq, _) when is_record(HttpReq, http_request) ->
	?HANDLE_DEBUG_LOG_N_RETURN([<<"POST">>, HttpReq], {stop, badRequestResponse()});

prepareResponseHeaders(Meth, Any, Smth) ->
	?HANDLE_ERROR_LOG_N_RETURN([Meth, Any, Smth], {error, "unexpected args"}).


headersResponse(HttpReq, AbsPath) when is_record(HttpReq, http_request) and is_binary(AbsPath) ->
	?DEBUG_ENTRY_LOG([HttpReq, AbsPath]),
	Res = case file:read_file_info(AbsPath) of
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
	end,
	?DEBUG_EXIT_LOG_N_RETURN([HttpReq, AbsPath], Res).

extractContentTypeHeader([]) -> ?HANDLE_ERROR_LOG_N_RETURN([], "no_content_type");

extractContentTypeHeader([#header{key = "Content-Type", value = Type} | _Others]) ->
	?HANDLE_DEBUG_LOG_N_RETURN(["Content-Type"], Type);

extractContentTypeHeader([_Any | Headers]) -> extractContentTypeHeader(Headers).


okResponse(Size, Type) when Size >= 0 ->
	?DEBUG_ENTRY_LOG([Size, Type]),
	Res = simple_http:responseToBinary(
		simple_http:generate(
			#response_version{minor = 1, major = 1},
			200,
			"OK",
			Type,
			Size
		)
	),
	?DEBUG_EXIT_LOG_N_RETURN([Size, Type], Res).


badRequestResponse() ->
	?DEBUG_ENTRY_LOG([]),
	Res = simple_http:responseToBinary(
		simple_http:generate(
			#response_version{major = 1, minor = 1},
			400,
			"Bad request"
		)
	),
	?DEBUG_EXIT_LOG_N_RETURN([], Res).


notFoundResponse() ->
	?DEBUG_ENTRY_LOG([]),
	Res = simple_http:responseToBinary(
		simple_http:generate(
			#response_version{major = 1, minor = 1},
			404,
			"Not found"
		)
	),
	?DEBUG_EXIT_LOG_N_RETURN([], Res).
