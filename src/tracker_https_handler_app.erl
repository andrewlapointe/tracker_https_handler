-module(tracker_https_handler_app).
-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
    {<<"*">>, [
        %{"/", cowboystatic, {priv_file, db_access, "static/index.html"}},
        {"/", toppage_h, []}
        %{"/gfriends",get_friends_h,[]},
        %{"/pfriends",set_friends_h,[]},
        %{"/afriend",add_friend_h,[]}
    ]}
    ]),
    {ok, _} = cowboy:start_tls(https_listener, [
        {port, 443},
        {certfile, "/priv/fullchain.pem"},
        {keyfile, "/priv/privkey.pem"}
    ], 
    #{env => #{dispatch => Dispatch}}),
    tracker_https_handler_sup:start_link().
stop(_State) ->
    ok.