-module(register_form).
-export([init/2]).


init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            %% Read and parse the URL-encoded form data
            {Fields, Req1} = cowboy_req:read_urlencoded_body(Req0),
            %% Extract 'user_data' from the form fields
            UserData = maps:get(<<"user_data">>, Fields, <<"">>),
            %% Process the data (e.g., log it)
            io:format("Received data: ~s~n", [UserData]),
            %% Prepare the response
            ResponseBody = io_lib:format(
                "<html><body><h1>Thank you, you submitted: ~s</h1></body></html>",
                [UserData]
            ),
            %% Send the response
            Req2 = cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"text/html">>},
                ResponseBody,
                Req1
            ),
            {ok, Req2, State};
        _ ->
            %% Handle other methods
            Req1 = cowboy_req:reply(
                405,
                #{<<"content-type">> => <<"text/plain">>},
                <<"Method Not Allowed">>,
                Req0
            ),
            {ok, Req1, State}
    end.
