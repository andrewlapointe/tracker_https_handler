-module(tracker_https_handler_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    %% Ensure all dependencies are started
    ok = application:ensure_all_started(ranch),
    ok = application:ensure_all_started(cowlib),
    ok = application:ensure_all_started(cowboy),
    %% Start your supervisor
    tracker_https_handler_sup:start_link(),

    % Get the paths for the symbolic links
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
