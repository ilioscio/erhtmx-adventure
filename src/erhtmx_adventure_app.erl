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

    %% Start Cowboy HTTP server on port 8080
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    io:format("~n=== ERHTMX Adventure Server Started ===~n"),
    io:format("Visit http://localhost:8080 to play!~n~n"),

    %% Start the supervisor
    erhtmx_adventure_sup:start_link().

%%--------------------------------------------------------------------
%% @doc Stops the application.
%% Cleans up the HTTP listener.
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    cowboy:stop_listener(http_listener),
    ok.
