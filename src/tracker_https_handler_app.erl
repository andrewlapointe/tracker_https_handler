-module(tracker_https_handler_app).
-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
    {'_', [
        {"/", toppage_h, []}
    ]}
    ]),
    {ok, _} = cowboy:start_tls(https_listener, [
        {port, 443},
        {certfile, "/etc/letsencrypt/live/cowboy.kickbackcode.com/cert.pem"},
        {keyfile, "/etc/letsencrypt/live/cowboy.kickbackcode.com/privkey.pem"},
        {cacertfile, "/etc/letsencrypt/live/cowboy.kickbackcode.com/chain.pem"}
    ], 
    #{env => #{dispatch => Dispatch}}),
    tracker_https_handler_sup:start_link().


stop(_State) ->
    ok.