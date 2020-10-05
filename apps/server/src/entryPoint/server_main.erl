-module(server_main).
-author("aptyp potatoch").

%% behaviour
-behaviour(application).
-export([start/2, stop/1, f/0]).

%% includes
-include("../server_args.hrl").
-include("../include/types.hrl").
-include("./server_props.hrl").
-include("../supervisor/sup_props.hrl").
-include_lib("kernel/include/logger.hrl").

-spec start(Type :: atom(), ServerArgs :: #server_args{}) -> {ok, pid(), port()}.
start(normal, Args)
		-> ?LOG_NOTICE("starting server application with args: ~p~n", [Args]),
%%	logger:set_module_level([server_acceptor, simple_http], debug), ?LOG_DEBUG("test"),
	fprof:trace(start, "my_fprof.trace"),
	ServerArgs = #server_args{configPath = Args},
	{ok, ServerProps} = args_to_props(ServerArgs),
	{ok, ListenSock} = start_listen_socket(ServerProps),
	{ok, SupPid} = server_sup:start_link(to_sup_props(ServerProps, ListenSock)),
	{ok, SupPid, ListenSock}.
stop(ListenSocket) when is_port(ListenSocket) -> ?LOG_NOTICE("stopping server application"),
	fprof:trace(stop),
	close_listen_socket(ListenSocket).

%% internal functions
-spec args_to_props(Args) -> classic_return(#server_props{})
	when Args :: #server_args{}.

args_to_props(Args) when is_record(Args, server_args) -> ?LOG_DEBUG("args_to_props: ~p~n", [Args]),
	handle_read_file(
		file:read_file(Args#server_args.configPath)
	).

handle_read_file({ok, Binary}) -> ?LOG_DEBUG("handle_read_file: ok, ~p~n", [Binary]),
	Lines = binary:split(Binary, <<"\n">>),
	LinesWithoutComments = clean_comments(Lines),
	KWLists = format_lines(LinesWithoutComments),
	TrimmedKwLists = trim_kw(KWLists),
	{ok, generate_props(TrimmedKwLists)};
handle_read_file({error, Reason}) -> ?LOG_DEBUG("handle_read_file: error, ~p~n", [Reason]),
	{error, logger:error("read_file: ~p", [Reason])}.


generate_props(KVLists)
		-> ?LOG_DEBUG("generate_props/1: ~p~n", [KVLists]),
	generate_props(KVLists, #server_props{}).

generate_props([], Props)
		-> ?LOG_DEBUG("generate_props/2: ~p~n", [{[], Props}]),
	Props;
generate_props([KV | KVLists], Props)
		-> ?LOG_DEBUG("generate_props/2: ~p~n", [{KV, KVLists, Props}]),
	generate_props(KVLists, add_prop(KV, Props)).

add_prop([<<"port">> | [Value]], Props) ->
	#server_props{
		workerNum = Props#server_props.workerNum,
		root = Props#server_props.root,
		port = binary_to_integer(Value)
	};
add_prop([<<"worker_count">> | [Value]], Props) ->
	#server_props{
		workerNum = binary_to_integer(Value),
		root = Props#server_props.root,
		port = Props#server_props.port
	};
add_prop([<<"document_root">> | [Value]], Props) ->
	#server_props{
		workerNum = Props#server_props.workerNum,
		root = Value,
		port = Props#server_props.port
	};
add_prop([_Other | [_]], Props) ->
	Props.

clean_comments(Lines) -> ?LOG_DEBUG("clean_comments: ~p~n", [Lines]),
	lists:map(
		fun (Line) -> lists:nth(
			1,
			binary:split(
				Line,
				<<"#">>
			)
		)
		end,
		Lines
	).

format_lines(LinesWithoutComments) -> ?LOG_DEBUG("~s: ~p~n", [?FUNCTION_NAME, LinesWithoutComments]),
	lists:map(
		fun(CleanLine) -> string:split(
			CleanLine,
			" "
		)
		end,
		LinesWithoutComments
	).

trim_kw(KWLists) -> ?LOG_DEBUG("~s/~p: ~p~n", [?FUNCTION_NAME, ?FUNCTION_ARITY, KWLists]),
	lists:map(
		fun (KW) -> lists:map(
			fun (Word) -> string:trim(Word) end,
			KW
		) end,
		KWLists
	).

start_listen_socket(#server_props{port = Port}) -> ?LOG_DEBUG("~s/~p: ~p~n", [?FUNCTION_NAME, ?FUNCTION_ARITY, Port]),
	gen_tcp:listen(Port, [
		{mode, binary},
		{packet, 0},
		{active, false}
	]).

to_sup_props(#server_props{root = Root, workerNum = WorkerNum}, LSock)
	when is_port(LSock) -> ?LOG_DEBUG("~s/~p: ~p~n", [?FUNCTION_NAME, ?FUNCTION_ARITY, {Root, WorkerNum, LSock}]),
	#sup_start_props{
		root = Root,
		acceptorNum = WorkerNum,
		listenSock = LSock
	}.

close_listen_socket(ListenSocket)
	when is_port(ListenSocket)-> ?LOG_DEBUG("~s/~p: ~p~n", [?FUNCTION_NAME, ?FUNCTION_ARITY, ListenSocket]),
	gen_tcp:close(ListenSocket).


f() -> application:stop(server), halt().