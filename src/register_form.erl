-module(register_form).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    io:format("Received ~s request~n", [Method]),
    case Method of
        <<"POST">> ->
            %% Read the binary body of the request
            case cowboy_req:read_body(Req0) of
                {ok, BinaryData, _} ->
                    %% Attempt to register the package
                    try gen_server:call({registration_server, 'logic@146.190.145.34'}, {register_package, BinaryData}) of
                        {ok, "Package registered", PackageKey} ->
                            io:format("Package registered with ID: ~s~n", [PackageKey]),
                            %% Respond with success
                            Headers = #{<<"content-type">> => <<"text/plain">>},
                            ReplyBody = <<"Package registered successfully! Package ID: ">> ++ PackageKey,
                            Req1 = cowboy_req:reply(200, Headers, ReplyBody, Req0),
                            {ok, Req1, State};
                        {error, Reason} ->
                            io:format("Failed to register package: ~p~n", [Reason]),
                            Headers = #{<<"content-type">> => <<"text/plain">>},
                            ReplyBody = <<"Failed to register package. Reason: ">> ++ atom_to_binary(Reason, utf8),
                            Req1 = cowboy_req:reply(500, Headers, ReplyBody, Req0),
                            {ok, Req1, State}
                    catch
                        _:Error ->
                            io:format("Unexpected error during registration: ~p~n", [Error]),
                            Headers = #{<<"content-type">> => <<"text/plain">>},
                            Req1 = cowboy_req:reply(500, Headers, <<"Internal Server Error">>, Req0),
                            {ok, Req1, State}
                    end;
                {error, Reason} ->
                    %% Handle errors in reading the body
                    io:format("Error reading body: ~p~n", [Reason]),
                    Headers = #{<<"content-type">> => <<"text/plain">>},
                    ReplyBody = <<"Unable to process request. Reason: ">> ++ atom_to_binary(Reason, utf8),
                    Req1 = cowboy_req:reply(400, Headers, ReplyBody, Req0),
                    {ok, Req1, State}
            end;
        _ ->
            %% Handle unsupported HTTP methods
            Headers = #{<<"content-type">> => <<"text/plain">>},
            Req1 = cowboy_req:reply(
                405,
                Headers,
                <<"Method Not Allowed">>,
                Req0
            ),
            {ok, Req1, State}
    end.
