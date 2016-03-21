-module(myrpc).
-export([start/0, loop/0, reply/4, call/4]).

% First function that is called when this Mod is called.
start() ->
    % Create a new process and register it with the name 'myrpc'. The new process
    % will start the function 'loop' with no arguments.
    register(myrpc, spawn(myrpc, loop, [])).

% The server code.
loop() ->
    receive
        % Receive message from process Cli.
        {Cli, {apply, Mod, Func, Args}} ->
            % Create new process on node myrpc and call remotely function 'reply'.
            file:write_file("/tmp/myrpc", io:fwrite("~p: Received message ~p from client\n", [self(), {apply, Mod, Func, Args}])),
            spawn(myrpc, reply, [Cli, Mod, Func, Args]),
            loop()
    end.

% Client function used to send the RPC request to the right server for eval.
reply(Client, Mod, Func, Args) ->
    Client ! {myrpc, catch apply(Mod, Func, Args)}.

call(Node, Mod, Func, Args) ->
    monitor_node(Node, true),
    io:fwrite("~p: Connected to node ~p\n", [self(), Node]),
    % Send a message to process 'myrpc' from node Node
    {myrpc, Node} ! {self(), {apply, Mod, Func, Args}},
    io:fwrite("~p: Sent message to node ~p\n", [self(), Node]),
    receive
        {myrpc, What} ->
            io:fwrite("~p: Received back message ~p from node ~p\n",
                    [self(), What, Node]),
            monitor_node(Node, false),
            What;
        {'EXIT', Node, Reason} ->
            io:fwrite("~p: Received EXIT from node ~p\n", [self(), Node]),
            exit(Reason);
        {nodedown, Node} ->
            error
    end.
