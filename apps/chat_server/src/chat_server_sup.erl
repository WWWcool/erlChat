%%%-------------------------------------------------------------------
%% @doc chat_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init(list()) -> {'ok',{{'one_for_all',0,1},[]}}.
init([]) ->
    SrvChild = {server, {server, start_link, []},
                permanent, 2000, worker, [server]},
    ClientChild = {client_local, {client_local, start_link, []},
                permanent, 2000, worker, [client_local]},
    {ok, { {one_for_one, 0, 1}, [SrvChild, ClientChild]} }.

%%====================================================================
%% Internal functions
%%====================================================================
