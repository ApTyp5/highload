-module(server_sup).
-author("aptyp potatoch").

%% includes
-include("./sup_props.hrl").
-include("../acceptor/acceptor_props.hrl").
-include("../include/types.hrl").
-include("../include/debug_output.hrl").
-include_lib("kernel/include/logger.hrl").

%% behaviour
-behaviour(supervisor).
-export([init/1]).

init(AcceptorProps) when is_record(AcceptorProps, acceptor_props) ->
	?DEBUG_ENTRY_LOG(AcceptorProps),
	SupFlags = #{
		strategy => simple_one_for_one,
		intensity => 0,
		period => 1
	},
	ChildSpecs = [#{
		id => acceptor,
		start => {server_acceptor, start_link, []}
	}],
	?LOG_NOTICE("starting supervisor with flats ~p and children spec ~p~n", [SupFlags, ChildSpecs]),
	Res = {ok, {SupFlags, ChildSpecs}},
	?DEBUG_EXIT_LOG_N_RETURN(AcceptorProps, Res).

%% exports
-export([start_link/1]).

-spec start_link(Props :: #sup_start_props{}) -> classic_return(pid()).

start_link(Props) when is_record(Props, sup_start_props) ->
	?DEBUG_ENTRY_LOG(Props),
	AcceptorProps = to_acceptor_props(Props),
	?LOG_NOTICE("acceptor properties are ~p~n", [AcceptorProps]),
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, AcceptorProps),
	ok = start_acceptors(Props#sup_start_props.acceptorNum, AcceptorProps),
	Res = {ok, Pid},
	?DEBUG_EXIT_LOG_N_RETURN(Props, Res).

%% internal functions

to_acceptor_props(#sup_start_props{listenSock = ListenSock, root = Root}) ->
	?DEBUG_ENTRY_LOG({ListenSock, Root}),
	Res = #acceptor_props{
		listenSock = ListenSock,
		root = Root
	},
	?DEBUG_EXIT_LOG_N_RETURN({ListenSock, Root}, Res).


start_acceptors(0, Props) when is_record(Props, acceptor_props) -> ?HANDLE_DEBUG_LOG_N_RETURN([0, Props], ok);

start_acceptors(WorkerNum, Props) when is_record(Props, acceptor_props) ->
	?DEBUG_ENTRY_LOG([WorkerNum, Props]),
	{ok, Pid} = supervisor:start_child(?MODULE, [Props]),
	ok = gen_server:cast(Pid, start_accept),
	start_acceptors(WorkerNum - 1, Props);

start_acceptors(Smth, Any) -> ?HANDLE_ERROR_LOG_N_RETURN([Smth, Any]).
