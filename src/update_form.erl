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
            case parse_update_data(BinaryData) of
                {ok, #{<<"package_id">> := PackageId} = ParsedData} ->
                    %% Validate the package ID
                    io:format("Updating package with ID: ~s~n", [PackageId]),
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
                    ReplyBody = <<"Failed to process request. Reason: ", ReasonBinary/binary>>,
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

%% Function to parse incoming binary data
parse_update_data(BinaryData) ->
    try
        %% Convert binary to string
        StringData = binary_to_list(BinaryData),
        %% Replace '+' with space
        NormalizedData = lists:map(fun(Char) -> if Char =:= $+ -> $\s; true -> Char end end, StringData),
        %% Split into key-value pairs
        Pairs = string:tokens(NormalizedData, "&"),
        %% Parse into a map
        ParsedData = lists:foldl(fun parse_pair/2, #{}, Pairs),
        %% Check if package_id is present and valid
        case maps:get(<<"package_id">>, ParsedData, undefined) of
            undefined ->
                io:format("Missing package_id in data: ~p~n", [Pairs]),
                {error, missing_package_id};
            <<>> ->
                io:format("Empty package_id in data: ~p~n", [Pairs]),
                {error, empty_package_id};
            _ ->
                {ok, ParsedData}
        end
    catch
        Class:Reason ->
            io:format("Failed to parse data. Class: ~p, Reason: ~p~n", [Class, Reason]),
            {error, invalid_data}
    end.

parse_pair(Pair, Acc) ->
    case string:tokens(Pair, "=") of
        [Key, Value] ->
            %% Normalize key to lowercase and add to the map, allowing empty values
            NormalizedKey = binary:copy(string:to_lower(Key)),
            maps:put(NormalizedKey, binary:copy(Value), Acc);
        _ ->
            io:format("Ignoring invalid pair: ~p~n", [Pair]),
            Acc
    end.