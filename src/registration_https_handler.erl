-module(registration_https_handler).

-export([init/3, allowed_methods/2, content_types_accepted/2, terminate/3]).
-export([handle_post/2]).

%% Initialize the handler
init(_Transport, Req, _Opts) ->
    {cowboy_rest, Req, #{}}.

%% Define allowed HTTP methods
allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

%% Define content types the handler accepts
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, handle_post}], Req, State}.

%% Handle POST requests
handle_post(Req, State) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            %% Decode JSON body into a map
            case jsx:decode(Body, [return_maps]) of
                {error, _Reason} ->
                    %% Handle JSON parsing errors
                    {ok, Req3} = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>},
                                                  <<"Invalid JSON format">>, Req2),
                    {halt, Req3, State};
                PackageData ->
                    %% Call the registration logic
                    Response = registration_server:register_package(PackageData),
                    %% Prepare JSON response
                    {ok, JsonResponse} = jsx:encode(Response),
                    {ok, Req3} = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, JsonResponse, Req2),
                    {halt, Req3, State}
            end;
        {error, Reason} ->
            %% Handle body read errors
            {ok, Req3} = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>},
                                          <<"Error reading body">>, Req),
            {halt, Req3, State}
    end.

%% Handle cleanup (not needed in this case, but Cowboy requires it)
terminate(_Reason, _Req, _State) ->
    ok.
