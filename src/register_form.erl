-module(register_form).
-export([init/2]).
% -include_lib("cowboy/include/cowboy.hrl").


init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    io:format("Received ~s request~n", [Method]),
    case Method of
        <<"POST">> ->
            %% Read and parse the URL-encoded form data
            {ok, Fields, Req1} = cowboy_req:body_qs(Req0),
            %% Extract 'tracking-number' from the form fields
            TrackingNumber = proplists:get_value(<<"tracking-number">>, Fields, <<"">>),
            %% Process the data
            io:format("Received tracking number: ~s~n", [TrackingNumber]),
            %% Send a response back to the client
            ResponseBody = io_lib:format(
                "<html><body><h1>Tracking Number Received: ~s</h1></body></html>",
                [TrackingNumber]
            ),
            Req2 = cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"text/html">>},
                ResponseBody,
                Req1
            ),
            {ok, Req2, State};
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
