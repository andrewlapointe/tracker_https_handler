-module(update_h).
-export([init/2]).

init(Req0, Opts) ->
    FilePath = filename:join([code:priv_dir(tracker_https_handler), "pages", "update.html"]),
    case file:read_file(FilePath) of
        {ok, Data} ->
            Headers = #{<<"content-type">> => <<"text/html">>},
            Req1 = cowboy_req:reply(200, Headers, Data, Req0),
            {ok, Req1, Opts};
        {error, Reason} ->
            error_logger:error_msg("Failed to read file: ~p~n", [Reason]),
            ErrorBody = <<"Internal Server Error">>,
            ErrorHeaders = #{<<"content-type">> => <<"text/plain">>},
            Req1 = cowboy_req:reply(500, ErrorHeaders, ErrorBody, Req0),
            {ok, Req1, Opts}
    end.
