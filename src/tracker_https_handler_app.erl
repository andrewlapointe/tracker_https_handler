-module(tracker_https_handler_app).
-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    %% Get the priv directory for the current application
    PrivDir = code:priv_dir(tracker_https_handler_app),
    CertFile = filename:join([PrivDir, "https", "cert.pem"]),
    KeyFile = filename:join([PrivDir, "https", "privkey.pem"]),
    CACertFile = filename:join([PrivDir, "https", "chain.pem"]),

    Dispatch = cowboy_router:compile([
    {'_', [
        %{"/", cowboystatic, {priv_file, db_access, "static/index.html"}},
        {"/", toppage_h, []},
        {"/track", tracking_http_handler, []}

        %{"/gfriends",get_friends_h,[]},
        %{"/pfriends",set_friends_h,[]},
        %{"/afriend",add_friend_h,[]}
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