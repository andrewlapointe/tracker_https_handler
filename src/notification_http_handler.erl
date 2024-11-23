-module(notification_http_handler).
-export([init/2]).

init(Req0, Opts) ->
    %% Read the log file
    case file:read_file("notifications.log") of
        {ok, Body} ->
            %% Serve the content as plain text
            Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Body, Req0),
            {ok, Req, Opts};
        {error, Reason} ->
            %% Handle errors (e.g., file not found)
            Req = cowboy_req:reply(
                500, #{<<"content-type">> => <<"text/plain">>}, <<"Error reading log file">>, Req0
            ),
            {ok, Req, Opts}
    end.
