-module(register_form).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    io:format("Received ~s request~n", [Method]),
    case Method of
        <<"POST">> ->
            % Read the binary body
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            io:format("Received binary body: ~p~n", [Body]),

            % Extract the tracking number
            case extract_tracking_number(Body) of
                {ok, TrackingNumber} ->
                    io:format("Extracted tracking number: ~p~n", [TrackingNumber]),

                    %% Make a direct call to the remote gen_server
                    RemoteName = {tracking_server, 'logic@143.198.146.54'},
                    case gen_server:call(RemoteName, {get_status, TrackingNumber}) of
                        {ok, Status} ->
                            %% Respond with success, displaying the package status
                            Headers = #{<<"content-type">> => <<"text/plain">>},
                            io:format(Status),
                            ResponseBody = "Tracking request received. Status: " ++ Status,
                            Req2 = cowboy_req:reply(200, Headers, ResponseBody, Req1),
                            {ok, Req2, State};
                        {error, Reason} ->
                            io:format("Error fetching package status from remote server: ~p~n", [Reason]),
                            Headers = #{<<"content-type">> => <<"text/plain">>},
                            Req2 = cowboy_req:reply(500, Headers, <<"Failed to retrieve package status">>, Req1),
                            {ok, Req2, State}
                    end;

                {error, Reason} ->
                    io:format("Error extracting tracking number: ~p~n", [Reason]),
                    
                    %% Respond with error
                    Headers = #{<<"content-type">> => <<"text/plain">>},
                    Req2 = cowboy_req:reply(400, Headers, <<"Invalid Tracking Data">>, Req1),
                    {ok, Req2, State}
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

-spec extract_tracking_number(binary()) -> {ok, binary()} | {error, atom()}.
extract_tracking_number(BinaryBody) ->
    %% Implement your binary parsing logic here.
    %% Example: Assume tracking number is prefixed with <<"=">>.
    Prefix = <<"=">>,
    case binary:split(BinaryBody, Prefix, [global]) of
        [_, TrackingNumber | _] ->
            {ok, TrackingNumber};
        _ ->
            {error, not_found}
    end.
