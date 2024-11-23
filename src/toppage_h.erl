-module(toppage_h).
-export([init/2]).

% -define(MAIN_LOGIC_PID, main_logic).

init(Req0, Opts) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Request forwarded">>, Req0),
    {ok, Req, Opts}.


    
    % {ok, Data} = file:read_file("/public/pages/index.html"),
    % Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/html">>, Req0),
    % {ok, Req2} = cowboy_req:reply(200, #{}, Data, Req1),
    % {ok, Req2, Opts}.
