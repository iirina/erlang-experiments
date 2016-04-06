-module(tcp_rpc_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    case tcp_rpc_supervisor:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
                {error, Other}
    end.

stop(_State) ->
    ok.
