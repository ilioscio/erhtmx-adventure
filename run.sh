#!/usr/bin/env bash
# Run the ERHTMX Adventure game server

set -e

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Use local rebar3 if available, otherwise try system rebar3
if [ -x "$SCRIPT_DIR/rebar3" ]; then
    REBAR3="$SCRIPT_DIR/rebar3"
else
    REBAR3="rebar3"
fi

echo "Building ERHTMX Adventure..."
$REBAR3 compile

echo ""
echo "Starting server..."
$REBAR3 shell
