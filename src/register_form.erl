-module(register_form).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    io:format("Received ~s request~n", [Method]),
    case Method of
        <<"POST">> ->
            {ok, Data, _Body} = cowboy_req:read_body(Req0),
            %% Log the received data for debugging
            io:format("Received data: ~p~n", [Data]),
            Headers = #{<<"content-type">> => <<"text/binary">>},
            ReplyBody = <<"Package registered successfully!">>,
            Req1 = cowboy_req:reply(200, Headers, ReplyBody, Req0),
            {ok, Req1, State};
        _ ->
            %% For methods other than POST
            Headers = #{<<"content-type">> => <<"text/plain">>},
            Req1 = cowboy_req:reply(
                405,
                Headers,
                <<"Method Not Allowed">>,
                Req0
            ),
            {ok, Req1, State}
    end.
