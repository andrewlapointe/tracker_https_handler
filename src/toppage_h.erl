-module(toppage_h).
-export([init/2]).

% -define(MAIN_LOGIC_PID, main_logic).

init(Req0, Opts) ->
    % Extract request information, e.g., JSON body (if any)
    % {ok, Body, Req2} = cowboy_req:read_body(Req),
    
    % Send a message to the main logic droplet
    % ?MAIN_LOGIC_PID ! {request_received, Body},

    % Respond to the client
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Request forwarded">>, Req0),
    {ok, Req, Opts}.
