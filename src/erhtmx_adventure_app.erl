%%%-------------------------------------------------------------------
%%% @doc erhtmx_adventure_app
%%% Main application module for ERHTMX Adventure.
%%% This module starts the Cowboy HTTP server and initializes the game.
%%%
%%% The application architecture:
%%% - Cowboy serves HTTP requests
%%% - Game state is stored in cookies (client-side)
%%% - HTMX handles partial page updates
%%% - HTML5 Canvas renders the game map
%%% - JavaScript handles real-time game logic (movement, combat)
%%% @end
%%%-------------------------------------------------------------------
-module(erhtmx_adventure_app).
-behaviour(application).

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc Starts the application.
%% Sets up Cowboy HTTP server with routes for:
%% - / : Main game page
%% - /api/game : Game state API (GET/POST)
%% - /api/map : Map data API
%% - /static/* : Static files (JS, CSS)
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    %% Define the routes for our HTTP server
    %% Each route maps a URL path to a handler module
    Dispatch = cowboy_router:compile([
        {'_', [
            %% Main game page - serves the HTML template
            {"/", game_handler, []},

            %% Character creation page
            {"/create", create_handler, []},

            %% Game state API - handles saving/loading game state
            {"/api/game", game_api_handler, []},

            %% Map data API - returns map configuration
            {"/api/map/:area/:x/:y", map_api_handler, []},

            %% Static file serving (JS, CSS, images)
            {"/static/[...]", cowboy_static, {priv_dir, erhtmx_adventure, "static"}}
        ]}
    ]),

    %% Get port from environment or use default
    Port = application:get_env(erhtmx_adventure, port, 8080),

    %% First, ensure any existing listener with this name is stopped
    %% This handles the case where the app is being restarted
    _ = cowboy:stop_listener(http_listener),

    %% Start Cowboy HTTP server
    case cowboy:start_clear(
        http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ) of
        {ok, _} ->
            io:format("~n=== ERHTMX Adventure Server Started ===~n"),
            io:format("Visit http://localhost:~p to play!~n~n", [Port]),
            %% Start the supervisor
            erhtmx_adventure_sup:start_link();
        {error, eaddrinuse} ->
            io:format("~n~n*** ERROR: Port ~p is already in use! ***~n", [Port]),
            io:format("~nAnother process is using this port. To fix this:~n"),
            io:format("  1. Find the process: lsof -i :~p~n", [Port]),
            io:format("  2. Kill it: kill <PID>~n"),
            io:format("  3. Or use a different port by setting the 'port' config~n~n"),
            %% Return error to prevent app from starting in broken state
            {error, {port_in_use, Port}};
        {error, Reason} ->
            io:format("~nFailed to start HTTP server: ~p~n", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Stops the application.
%% Cleans up the HTTP listener.
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    cowboy:stop_listener(http_listener),
    ok.
