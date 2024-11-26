-module(register_form).
-export([init/2]).


init(Req0, State) ->
    %% Ensure the request method is POST
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            %% Check if content-type is application/json
            Headers = cowboy_req:headers(Req0),
            case maps:get(<<"content-type">>, Headers, <<"">>) of
                <<"application/json">> ->
                    {ok, Body, Req1} = cowboy_req:read_body(Req0),
                    %% Decode JSON
                    {ok, Json} = jsx:decode(Body, [return_maps]),
                    UserData = maps:get(<<"user_data">>, Json, <<"">>),
                    %% Process the data
                    io:format("Received JSON data: ~s~n", [UserData]),
                    %% Send a response
                    ResponseBody = <<"Received your JSON data!">>,
                    {ok, Req2} = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, ResponseBody, Req1),
                    {ok, Req2, State};
                _ ->
                    %% Unsupported Media Type
                    {ok, Req1} = cowboy_req:reply(415, #{<<"content-type">> => <<"text/plain">>}, <<"Unsupported Media Type">>, Req0),
                    {ok, Req1, State}
            end;
        _ ->
            {ok, Req0, State}
    end.