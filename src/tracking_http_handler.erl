-module(tracking_http_handler).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    %% Only handle GET requests to "/track/:package_id"
    Response = case {Method, Path} of
        {get, <<"/track/", PackageId/binary>>} ->
            %% Query the tracking_app gen_server to get the status
            case tracking_app:get_status(PackageId) of
                {ok, Data} ->
                    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{status => Data}), Req0);
                {error, "Package not found"} ->
                    cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, <<"Package not found">>, Req0);
                {error, Reason} ->
                    cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{error => Reason}), Req0)
            end;
        _ ->
            %% Respond with 404 Not Found for other methods/paths
            cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, <<"Not Found">>, Req0)
    end,

    {ok, Response, Opts}.
