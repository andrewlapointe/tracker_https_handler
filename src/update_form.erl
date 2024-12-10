-module(update_form).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    io:format("Received ~s request~n", [Method]),
    case Method of
        <<"POST">> ->
            %% Read the binary body of the request
            case cowboy_req:read_body(Req0) of
                {ok, BinaryData, _} ->
                    %% Extract package ID from the binary data
                    io:format("Received raw data: ~p~n", [BinaryData]),
                    case cowboy_req:parse_qs(Req0) of
                        {ok, #{<<"package_id">> := PackageId}} ->
                            %% Call the gen_server with the package ID and raw binary data
                            case gen_server:call({package_monitor_server, 'logic@146.190.145.34'}, {update_package, PackageId, BinaryData}) of
                                ok ->
                                    io:format("Package update initiated for ID: ~s~n", [binary_to_list(PackageId)]),
                                    %% Respond with a success message
                                    Headers = #{<<"content-type">> => <<"text/plain">>},
                                    ReplyBody = <<"Package update initiated successfully!">>,
                                    Req1 = cowboy_req:reply(200, Headers, ReplyBody, Req0),
                                    {ok, Req1, State};
                                {error, Reason} ->
                                    %% Handle errors in calling the gen_server
                                    io:format("Failed to update package: ~p~n", [Reason]),
                                    Headers = #{<<"content-type">> => <<"text/plain">>},
                                    ReasonBinary = atom_to_binary(Reason, utf8),
                                    ReplyBody = <<"Failed to update package. Reason: ", ReasonBinary/binary>>,
                                    Req1 = cowboy_req:reply(500, Headers, ReplyBody, Req0),
                                    {ok, Req1, State}
                            end;
                        _ ->
                            %% Handle missing package ID in query parameters
                            io:format("Package ID missing from request~n"),
                            Headers = #{<<"content-type">> => <<"text/plain">>},
                            ReplyBody = <<"Package ID is required for update.">>,
                            Req1 = cowboy_req:reply(400, Headers, ReplyBody, Req0),
                            {ok, Req1, State}
                    end;
                {error, Reason} ->
                    %% Handle errors in reading the body
                    io:format("Error reading body: ~p~n", [Reason]),
                    Headers = #{<<"content-type">> => <<"text/plain">>},
                    ReasonBinary = atom_to_binary(Reason, utf8),
                    ReplyBody = <<"Unable to process request. Reason: ", ReasonBinary/binary>>>,
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
