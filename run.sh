#!/usr/bin/env bash
# Run the ERHTMX Adventure game server

set -e

echo "Building ERHTMX Adventure..."
rebar3 compile

echo ""
echo "Starting server..."
rebar3 shell
