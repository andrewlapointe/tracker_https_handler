-module(utils).
-export([get_node_name/0]).

get_node_name() ->
    % Get the node name
    node().