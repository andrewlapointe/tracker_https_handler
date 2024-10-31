-module(tracker_https_handler_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start the Cowboy router
    {ok, _} = cowboy:start_https(my_https_listener, 100,
        #{port => 8443,
          certfile => "/path/to/cert.pem",
          keyfile => "/path/to/key.pem"},
        #{env => #{dispatch => cowboy_router:compile([
              {'_', [
                  {"/", cowboy_handler, []}
              ]}
          ])}}
    ),
    {ok, self()}.

stop(_State) ->
    ok.
