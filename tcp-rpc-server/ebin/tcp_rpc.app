{application, tcp_rpc,
    [{description, "RPC TCP server"},
     {vsn, "0.1.0"},
     {modules, [
        tcp_rpc_app,
        tcp_rpc_server,
        tcp_rpc_supervisor
     ]},
     {registered, [tcp_rpc_supervisor, tcp_rpc_server]},
     {applications, [kernel, stdlib]},
     {mod, {tcp_rpc_app, []}}
    ]
}.
