-module(tracker_https_handler_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    CertFile = "priv/https/cert.pem", 
    KeyFile = "priv/https/privkey.pem", 
    CACertFile = "priv/https/chain.pem",

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", toppage_h, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_tls(https_listener, [
        {port, 443},
        {certfile, CertFile},
        {keyfile, KeyFile},
        {cacertfile, CACertFile}
    ], 
    #{env => #{dispatch => Dispatch}}),

    tracker_https_handler_sup:start_link().

stop(_State) ->
    ok.
