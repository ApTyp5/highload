-module(server_main).
-author("aptyp potatoch").

%% includes
-include("../server_args.hrl").
-include("../include/types.hrl").
-include("../include/debug_output.hrl").
-include("./server_props.hrl").
-include("../supervisor/sup_props.hrl").
-include_lib("kernel/include/logger.hrl").

%% exports
-export([stop_server/0]).

%% behaviour
-behaviour(application).
-export([start/2, stop/1]).

-spec start(Type :: atom(), ServerArgs :: #server_args{}) -> {ok, pid(), port()}.

start(normal, Args) ->
	?LOG_NOTICE("starting server application with args: ~p~n", [Args]),
%%	logger:set_module_level([server_main], debug), ?LOG_DEBUG("test"),
	ServerArgs = #server_args{configPath = Args},
	{ok, ServerProps} = args_to_props(ServerArgs),
	?LOG_NOTICE("server props are: ~p~n", [ServerProps]),
	{ok, ListenSock} = start_listen_socket(ServerProps),
	{ok, SupPid} = server_sup:start_link(to_sup_props(ServerProps, ListenSock)),
	{ok, SupPid, ListenSock}.

stop(ListenSocket) when is_port(ListenSocket) ->
	?LOG_NOTICE("stopping server application"),
	close_listen_socket(ListenSocket).

%% internal functions
-spec args_to_props(Args) -> classic_return(#server_props{})
	when Args :: #server_args{}.

args_to_props(Args) when is_record(Args, server_args) ->
	?DEBUG_ENTRY_LOG(Args),
	Res = handle_read_file(
		file:read_file(Args#server_args.configPath)
	),
	?DEBUG_EXIT_LOG_N_RETURN(Args, Res).


handle_read_file({ok, Binary}) ->
	?DEBUG_ENTRY_LOG({ok, Binary}),
	Lines = binary:split(Binary, <<"\n">>, [global]),
	LinesWithoutComments = clean_comments(Lines),
	KWLists = format_lines(LinesWithoutComments),
	TrimmedKwLists = trim_kw(KWLists),
	?DEBUG_EXIT_LOG_N_RETURN({ok, Binary}, {ok, generate_props(TrimmedKwLists)});

handle_read_file(Error) -> ?HANDLE_ERROR_LOG_N_RETURN(Error).


generate_props(KVLists) ->
	?DEBUG_ENTRY_LOG(KVLists),
	Res = generate_props(KVLists, #server_props{}),
	?DEBUG_EXIT_LOG_N_RETURN(KVLists, Res).


generate_props([], Props) -> ?HANDLE_DEBUG_LOG_N_RETURN([[], Props], Props);

generate_props([KV | KVLists], Props) ->
	?DEBUG_ENTRY_LOG([[KV | KVLists], Props]),
	Res = generate_props(KVLists, add_prop(KV, Props)),
	?DEBUG_EXIT_LOG_N_RETURN([[KV | KVLists], Props], Res).


add_prop([<<"port">> | [Value]], Props) ->
	?DEBUG_ENTRY_LOG([[<<"port">> | [Value]], Props]),
	Res = #server_props{
		workerNum = Props#server_props.workerNum,
		root = Props#server_props.root,
		port = binary_to_integer(Value)
	},
	?DEBUG_EXIT_LOG_N_RETURN([[<<"port">> | [Value]], Props], Res);

add_prop([<<"worker_count">> | [Value]], Props) ->
	?DEBUG_ENTRY_LOG([[<<"worker_count">> | [Value]], Props]),
	Res = #server_props{
		workerNum = binary_to_integer(Value),
		root = Props#server_props.root,
		port = Props#server_props.port
	},
	?DEBUG_EXIT_LOG_N_RETURN([[<<"worker_count">> | [Value]], Props], Res);

add_prop([<<"document_root">> | [Value]], Props) ->
	?DEBUG_ENTRY_LOG([[<<"document_root">> | [Value]], Props]),
	Res = #server_props{
		workerNum = Props#server_props.workerNum,
		root = Value,
		port = Props#server_props.port
	},
	?DEBUG_EXIT_LOG_N_RETURN([[<<"document_root">> | [Value]], Props], Res);

add_prop([ErrorProp], Props) -> ?HANDLE_ERROR_LOG_N_RETURN([[ErrorProp], Props], Props).


clean_comments(Lines) ->
	?DEBUG_ENTRY_LOG(Lines),
	Res = lists:map(
		fun (Line) -> lists:nth(
			1,
			binary:split(
				Line,
				<<"#">>
			)
		)
		end,
		Lines
	),
	?DEBUG_EXIT_LOG_N_RETURN(Lines, Res).


format_lines(LinesWithoutComments) ->
	?DEBUG_ENTRY_LOG(LinesWithoutComments),
	Res = lists:map(
		fun(CleanLine) -> string:split(
			CleanLine,
			" "
		)
		end,
		LinesWithoutComments
	),
	?DEBUG_EXIT_LOG_N_RETURN(LinesWithoutComments, Res).


trim_kw(KWLists) ->
	?DEBUG_ENTRY_LOG(KWLists),
	Res = lists:map(
		fun (KW) -> lists:map(
			fun (Word) -> string:trim(Word) end,
			KW
		) end,
		KWLists
	),
	?DEBUG_EXIT_LOG_N_RETURN(KWLists, Res).


start_listen_socket(#server_props{port = Port}) ->
	?DEBUG_ENTRY_LOG(Port),
	Options = [
		{mode, binary},
		{packet, 0},
		{active, false},
		{buffer, 1024 * 1024 * 1024},
		{recbuf, 1024 * 1024 * 512},
		{sndbuf, 1024 * 1024 * 512},
		{show_econnreset, true},
		{backlog, 2048}
	],
	?LOG_NOTICE("start listening on port ~p with options ~p~n", [Port, Options]),
	Res = gen_tcp:listen(Port, Options),
	?DEBUG_EXIT_LOG_N_RETURN(Port, Res).

to_sup_props(#server_props{root = Root, workerNum = WorkerNum}, LSock)
	when is_port(LSock) ->
	?DEBUG_ENTRY_LOG([{Root, WorkerNum}, LSock]),
	Res = #sup_start_props{
		root = Root,
		acceptorNum = WorkerNum,
		listenSock = LSock
	},
	?DEBUG_EXIT_LOG_N_RETURN([{Root, WorkerNum}, LSock], Res).


close_listen_socket(ListenSocket)
	when is_port(ListenSocket)->
	?DEBUG_ENTRY_LOG(ListenSocket),
	Res = gen_tcp:close(ListenSocket),
	?DEBUG_EXIT_LOG_N_RETURN(ListenSocket, Res).


stop_server() -> application:stop(server), halt().
