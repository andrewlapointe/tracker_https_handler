-module(register_form).
-export([init/2]).


init(Req0, State) ->
    %% Ensure the request method is POST
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            %% Get the Content-Type header
            Headers = cowboy_req:headers(Req0),
            ContentType = maps:get(<<"content-type">>, Headers, <<"">>),
            %% Check if Content-Type is application/x-www-form-urlencoded
            case ContentType of
                <<"application/x-www-form-urlencoded">> ->
                    %% Read the POST body
                    {ok, Body, Req1} = cowboy_req:read_body(Req0),
                    %% Parse form data
                    FormData = cowboy_http:parse_qs(Body),
                    %% Extract the value associated with 'user_data'
                    UserData = proplists:get_value(<<"user_data">>, FormData, <<"">>),
                    %% Process the data
                    io:format("Received data: ~s~n", [UserData]),
                    %% Send a response back to the client
                    ResponseBody = <<"Thank you for your submission!">>,
                    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, ResponseBody, Req1),
                    {ok, Req2, State};
                _ ->
                    %% Unsupported Media Type
                    Req1 = cowboy_req:reply(
                        415,
                        #{<<"content-type">> => <<"text/plain">>},
                        <<"Unsupported Media Type">>,
                        Req0
                    ),
                    {ok, Req1, State}
            end;
        _ ->
            %% Handle other HTTP methods if necessary
            {ok, Req0, State}
    end.
