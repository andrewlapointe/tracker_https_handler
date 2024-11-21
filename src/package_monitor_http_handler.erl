-module(package_monitor_http_handler).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    %% Only handle POST requests to "/update"
    Response = case {Method, Path} of
        {post, "/update"} ->
            %% Read the JSON body
            case cowboy_req:read_body(Req0) of
                {ok, Body, Req1} ->
                    %% Decode JSON into a map
                    case jsx:decode(Body, [return_maps]) of
                        {ok, JsonMap} ->
                            %% Extract Package ID and Update Data
                            PackageId = maps:get(package_id, JsonMap),
                            UpdateData = maps:remove(package_id, JsonMap),
                            %% Call package_monitor_app to update the package
                            package_monitor_app:update_package(PackageId, UpdateData),
                            %% Respond with 202 Accepted to indicate the update was processed
                            cowboy_req:reply(202, #{<<"content-type">> => <<"application/json">>}, <<"Package update received">>, Req1);
                        {error, DecodeReason} ->
                            io:format("JSON decode error: ~p~n", [DecodeReason]),
                            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"Invalid JSON">>, Req1)
                    end;
                {error, ReadError} ->
                    io:format("Request body read error: ~p~n", [ReadError]),
                    cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"Failed to read request body">>, Req0)
            end;
        _ ->
            %% Respond with 404 Not Found for other methods/paths
            cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, <<"Not Found">>, Req0)
    end,

    {ok, Response, Opts}.
