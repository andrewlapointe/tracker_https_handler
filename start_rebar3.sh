# #!/bin/bash
# # Ensure environment variables are set
# if [ -z "$NODE_NAME" ]; then
#   echo "Please set the NODE_NAME environment variable."
#   exit 1
# fi

# if [ -z "$COOKIE" ]; then
#   echo "Please set the COOKIE environment variable."
#   exit 1
# fi

# Run the rebar3 shell with the specified node name and cookie
# sudo -E rebar3 shell --name $NODE_NAME --setcookie $COOKIE

sudo -E rebar3 shell --name cowboy@24.199.101.193 --setcookie MomsHomadeCookies