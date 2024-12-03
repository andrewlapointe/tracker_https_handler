-module(register_form).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    io:format("Received request: ~p~n", [Req0]),
    io:format("Received ~s request~n", [Method]),
    case Method of
        <<"POST">> ->
            %% Read and parse the URL-encoded form data
            case cowboy_req:body_qs(Req0) of
                {ok, Fields, Req1} ->
                    %% Extract 'tracking-number' from the form fields
                    TrackingNumber = proplists:get_value(<<"tracking-number">>, Fields, <<"">>),
                    io:format("Received tracking number: ~s~n", [TrackingNumber]),
                    %% Send a response back to the client
                    ResponseBody = iolist_to_binary(
                        io_lib:format(
                            "<html><body><h1>Tracking Number Received: ~s</h1></body></html>",
                            [TrackingNumber]
                        )
                    ),
                    Req2 = cowboy_req:reply(
                        200,
                        #{<<"content-type">> => <<"text/html">>},
                        ResponseBody,
                        Req1
                    ),
                    {ok, Req2, State};
                {error, Reason} ->
                    io:format("Error parsing form data: ~p~n", [Reason]),
                    Req1 = cowboy_req:reply(
                        400,
                        #{<<"content-type">> => <<"text/plain">>},
                        <<"Bad Request">>,
                        Req0
                    ),
                    {ok, Req1, State}
            end;
        _ ->
            %% For methods other than POST
            Req1 = cowboy_req:reply(
                405,
                #{<<"content-type">> => <<"text/plain">>},
                <<"Method Not Allowed">>,
                Req0
            ),
            {ok, Req1, State}
    end.
