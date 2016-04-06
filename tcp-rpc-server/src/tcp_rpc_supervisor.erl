-module(tcp_rpc_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export[init/1].

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Server = {tcp_rpc_server, {tcp_rpc_server, start_link, []}, permanent,
                2000, worker, [tcp_rpc_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
