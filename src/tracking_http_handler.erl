-module(tracking_http_handler).
-export([init/2]).

init(Req0, Opts) ->
    %% Read and decode the request body
    {ok, Data, _} = cowboy_req:read_body(Req0),
    case jsx:decode(Data, [return_maps]) of
        {ok, #{<<"package_id">> := 1}} ->
            %% Call the tracking_app GenServer to get the status
            case gen_server:call({tracking_server, utils:get_node_name()}, {get_status, 1}) of
                {ok, Status} ->
                    %% Respond with the tracking status as JSON
                    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{status => Status}), Req0),
                    {ok, Req, Opts};
                {error, Reason} ->
                    %% Respond with an error message
                    Req = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{error => Reason}), Req0),
                    {ok, Req, Opts}
            end;
        _ ->
            %% Handle invalid JSON structure
            Req = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{error => <<"Invalid JSON or missing 'package_id'">>}), Req0),
            {ok, Req, Opts}
    end.
