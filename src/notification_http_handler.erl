-module(notification_http_handler).
-export([init/3, handle/2]).

init(_, Req, State) ->
    {cowboy_rest, Req, State}.

handle(Req, State) ->
    %% Read and decode the JSON payload
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case jsx:decode(Body) of
        {ok, #{<<"package_id">> := PackageId, <<"status">> := Status}} ->
            io:format("Received notification: PackageId=~p, Status=~p~n", [PackageId, Status]),
            {ok, Req2, State};
        _ ->
            {error, Req2, State}
    end.
