-module(toppage_h).
-export([init/2]).

% -define(MAIN_LOGIC_PID, main_logic).

init(Req0, Opts) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Request forwarded">>, Req0),
    {ok, Req, Opts}.
