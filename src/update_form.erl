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
                    io:format("Received raw data: ~p~n", [BinaryData]),
                    %% Extract the package_id from the body
                    case parse_package_id(BinaryData) of
                        {ok, PackageId} ->
                            %% Call the gen_server with the package ID and raw binary data
                            case gen_server:cast({package_monitor_server, 'logic@146.190.145.34'}, {update_package, PackageId, BinaryData}) of
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
                        {error, Reason} ->
                            %% Handle missing or invalid package ID
                            io:format("Failed to extract package ID: ~p~n", [Reason]),
                            Headers = #{<<"content-type">> => <<"text/plain">>},
                            ReplyBody = <<"Package ID is missing or invalid.">>,
                            Req1 = cowboy_req:reply(400, Headers, ReplyBody, Req0),
                            {ok, Req1, State}
                    end;
                {error, Reason} ->
                    %% Handle errors in reading the body
                    io:format("Error reading body: ~p~n", [Reason]),
                    Headers = #{<<"content-type">> => <<"text/plain">>},
                    ReasonBinary = atom_to_binary(Reason, utf8),
                    ReplyBody = <<"Unable to process request. Reason: ", ReasonBinary/binary>>,
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

%% Function to extract package_id from the body
parse_package_id(BinaryData) ->
    try
        %% Convert binary to string for processing
        StringData = binary_to_list(BinaryData),
        %% Replace '+' with space
        NormalizedData = lists:map(fun(Char) -> if Char =:= $+ -> $\s; true -> Char end end, StringData),
        %% Split by '&' into key-value pairs
        Pairs = string:tokens(NormalizedData, "&"),
        %% Convert key-value pairs into a map
        ParsedMap = lists:foldl(fun parse_pair/2, #{}, Pairs),
        %% Find the package_id key
        case maps:get("package_id", ParsedMap, undefined) of
            undefined -> {error, missing_package_id};
            PackageId -> {ok, binary:copy(PackageId)}
        end
    catch
        _:Error -> {error, invalid_data}
    end.

%% Helper function to parse a single key-value pair into a map
parse_pair(Pair, Acc) ->
    case string:tokens(Pair, "=") of
        [Key, Value] -> maps:put(Key, Value, Acc);
        _ -> Acc
    end.
