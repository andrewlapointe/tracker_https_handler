-module(update_form).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    io:format("Received ~s request~n", [Method]),
    case Method of
        <<"POST">> ->
            {ok, BinaryData, _Body} = cowboy_req:read_body(Req0),
            io:format("Received data: ~p~n", [BinaryData]),
            %% Parse the received data
            case parse_package_data(BinaryData) of
                {ok, #{<<"package_id">> := PackageId} = ParsedData} ->
                    %% Validate the package ID
                    io:format("Updating package with ID: ~s~n", [binary_to_list(PackageId)]),
                    %% Call the gen_server with the parsed data
                    package_monitor_server:update_db_record(PackageId, ParsedData),
                    %% Respond with success
                    Headers = #{<<"content-type">> => <<"text/plain">>},
                    ReplyBody = <<"Package update initiated successfully!">>,
                    Req1 = cowboy_req:reply(200, Headers, ReplyBody, Req0),
                    {ok, Req1, State};
                {error, Reason} ->
                    %% Handle parsing or validation error
                    io:format("Failed to parse or validate data: ~p~n", [Reason]),
                    Headers = #{<<"content-type">> => <<"text/plain">>},
                    ReasonBinary = atom_to_binary(Reason, utf8),
                    ReplyBody = <<"Failed to register package. Reason: ", ReasonBinary/binary>>,
                    Req1 = cowboy_req:reply(400, Headers, ReplyBody, Req0),
                    {ok, Req1, State}
            end;
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

%% Parse the received data into a map with binary keys and values
parse_package_data(BinaryData) ->
    try
        %% Convert binary to string for processing
        StringData = binary_to_list(BinaryData),
        %% Replace '+' with space
        NormalizedData = lists:map(fun(Char) -> if Char =:= $+ -> $\s; true -> Char end end, StringData),
        %% Split by '&' into key-value pairs
        Pairs = string:tokens(NormalizedData, "&"),
        %% Parse each key-value pair into a map
        ParsedData = lists:foldl(fun parse_pair/2, #{}, Pairs),
        {ok, ParsedData}
    catch
        _:Error ->
            io:format("Failed to parse data.~n"),
            {error, invalid_data}
    end.

%% Parse a single key-value pair and add it to the map
parse_pair(Pair, Acc) ->
    case string:tokens(Pair, "=") of
        [Key, Value] ->
            DecodedKey = binary:copy(decode_url(Key)),
            DecodedValue = binary:copy(decode_url(Value)),
            maps:put(DecodedKey, DecodedValue, Acc);
        _ ->
            io:format("Skipping invalid pair: ~p~n", [Pair]),
            Acc
    end.

%% Decode URL-encoded values (handles '+' and percent-encoding)
decode_url(Value) ->
    try
        uri_string:decode(Value)
    catch
        _:Error -> binary:copy(Value) % Ensure binary output in case of failure
    end.
