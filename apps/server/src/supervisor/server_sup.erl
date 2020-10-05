%%%-------------------------------------------------------------------
%% @doc server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(server_sup).

-behaviour(supervisor).

-include("./sup_props.hrl").
-include("../acceptor/acceptor_props.hrl").
-include("../include/types.hrl").
-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).

-export([init/1]).

-spec start_link(Props :: #sup_start_props{}) -> classic_return(pid()).
start_link(Props)
		when is_record(Props, sup_start_props)
		-> ?LOG_NOTICE("starting server supervisor with props: ~p~n", [Props]),
	AcceptorProps = to_acceptor_props(Props),
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, AcceptorProps),
	?LOG_DEBUG("acceptor args: ~p~n", [{Props#sup_start_props.acceptorNum, AcceptorProps}]),
	OK = start_acceptors(Props#sup_start_props.acceptorNum, AcceptorProps),
	?LOG_DEBUG("acceptor retval: ~p~n", [OK]),
	{ok, Pid}.

init(AcceptorProps)
		when is_record(AcceptorProps, acceptor_props)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, AcceptorProps]),
	SupFlags = #{
		strategy => simple_one_for_one,
		intensity => 0,
		period => 1
	},
	ChildSpecs = [#{
		id => acceptor,
		start => {server_acceptor, start_link, []}
	}],
	{ok, {SupFlags, ChildSpecs}}.

%% internal functions

to_acceptor_props(#sup_start_props{listenSock = ListenSock, root = Root})
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, {ListenSock, Root}]),
	#acceptor_props{
		listenSock = ListenSock,
		root = Root
	}.

start_acceptors(0, Props) when is_record(Props, acceptor_props)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [0, Props]]),
	ok;
start_acceptors(WorkerNum, Props)
		when is_record(Props, acceptor_props)
		-> ?LOG_DEBUG("~s:~s/~p: ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [WorkerNum, Props]]),
	{ok, Pid} = supervisor:start_child(?MODULE, [Props]),
	?LOG_DEBUG("child pid = ~p~n", [Pid]),
	ok = gen_server:cast(Pid, start_accept),
	start_acceptors(WorkerNum - 1, Props);
start_acceptors(Smth, Any) ->
	ErrorMessage = io_lib:format("unexpected ~s:~s/~p(~p)~n", [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY, [Smth, Any]]),
	?LOG_ERROR(ErrorMessage),
	{error, ErrorMessage}.
