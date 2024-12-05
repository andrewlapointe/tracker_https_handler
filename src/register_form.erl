-module(register_form).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    io:format("Received ~s request~n", [Method]),
    case Method of
        <<"POST">> ->
           Body = cowboy_req:read_body(Req0),
            Headers = #{<<"content-type">> => <<"text/html">>},
            Req1 = cowboy_req:reply(200, Headers, Body, Req0),
            {ok, Req1, State};

        _ ->
            %% For methods other than POST
            Req1 = cowboy_req:reply(
                405,
                [{<<"content-type">>, <<"text/plain">>}],
                <<"Method Not Allowed">>,
                Req0
            ),
            {ok, Req1, State}
    end.
