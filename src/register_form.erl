-module(register_form).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    io:format("Received ~s request~n", [Method]),
    case Method of
        <<"POST">> ->

            %% Read the request body
            case cowboy_req:read_body(Req0) of
                {ok, _Body, Req1} ->
                    % %% Parse the URL-encoded form data using http_uri:parse_query/1
                    % Fields = http_uri:parse_query(binary_to_list(Body)),
                    % %% Extract 'tracking-number' from the form fields
                    % TrackingNumber = proplists:get_value("tracking-number", Fields, ""),
                    % io:format("Received tracking number: ~s~n", [TrackingNumber]),
                    % %% Send a response back to the client
                    % QueryString = cowboy_req:qs(Req1),
                    ParsedQs = binary_to_list(cowboy_req:qs(Req1)),
                    % AtomsQs = [{binary_to_existing_atom(K, latin1), V}
                    %     || {K, V} <- ParsedQs],
                    Headers = #{<<"content-type">> => <<"text/html">>},
                    ResponseBody = io_lib:format("<html><body><h1>Tracking Number Received: ~p</h1></body></html>", [ParsedQs]),
                    Req2 = cowboy_req:reply(
                        200,
                        Headers,
                        ResponseBody,
                        Req1
                    ),
                    {ok, Req2, State};
                {more, _, _} ->
                    %% Handle cases where the body is too large or needs to be read in chunks
                    Req1 = cowboy_req:reply(
                        413,
                        [{<<"content-type">>, <<"text/plain">>}],
                        <<"Payload Too Large">>,
                        Req0
                    ),
                    {ok, Req1, State};
                {error, Reason} ->
                    io:format("Error reading body: ~p~n", [Reason]),
                    Req1 = cowboy_req:reply(
                        400,
                        [{<<"content-type">>, <<"text/plain">>}],
                        <<"Bad Request">>,
                        Req0
                    ),
                    {ok, Req1, State}
            end;
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
