%%%-------------------------------------------------------------------
%%% @doc game_handler
%%% HTTP handler for the main game page.
%%%
%%% This handler serves the HTML page that contains:
%%% - The game canvas for rendering the map and entities
%%% - HTMX-powered UI elements (character stats, inventory)
%%% - JavaScript game engine
%%%
%%% The handler reads game state from cookies and renders the
%%% appropriate page (character creation or game screen).
%%% @end
%%%-------------------------------------------------------------------
-module(game_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handles GET requests to the root path.
%% Checks for existing game state in cookies:
%% - If no state or not initialized: redirect to character creation
%% - If valid state: render game page
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    %% Try to get game state from cookie
    Cookies = cowboy_req:parse_cookies(Req0),
    GameState = case lists:keyfind(<<"game_state">>, 1, Cookies) of
        {_, EncodedState} ->
            %% URL decode and parse JSON
            try
                Decoded = uri_string:percent_decode(EncodedState),
                case game_state:from_json(Decoded) of
                    {error, _} -> game_state:default_state();
                    ValidState -> ValidState
                end
            catch
                _:_ -> game_state:default_state()
            end;
        false ->
            game_state:default_state()
    end,

    %% Check if player has been created
    case maps:get(initialized, GameState, false) of
        false ->
            %% Redirect to character creation
            Req = cowboy_req:reply(302, #{
                <<"location">> => <<"/create">>
            }, <<>>, Req0),
            {ok, Req, State};
        _ ->
            %% Render game page
            Html = render_game_page(GameState),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/html">>
            }, Html, Req0),
            {ok, Req, State}
    end.

%%--------------------------------------------------------------------
%% @doc Renders the main game HTML page.
%% @end
%%--------------------------------------------------------------------
render_game_page(GameState) ->
    Name = maps:get(name, GameState, <<"Hero">>),
    Class = maps:get(class, GameState, warrior_sword),
    HP = maps:get(hp, GameState, 100),
    MaxHP = maps:get(max_hp, GameState, 100),
    Area = maps:get(area, GameState, training_grounds),
    MapX = maps:get(map_x, GameState, 0),
    MapY = maps:get(map_y, GameState, 0),
    Inventory = maps:get(inventory, GameState, []),

    %% Convert inventory to JSON for JavaScript
    InventoryJson = jsx:encode(Inventory),

    %% Get map data
    MapTiles = map_data:get_map(Area, MapX, MapY),
    MapJson = jsx:encode(MapTiles),

    Enemies = map_data:get_enemies(Area, MapX, MapY),
    EnemiesJson = jsx:encode(format_enemies(Enemies)),

    Doors = map_data:get_doors(Area, MapX, MapY),
    DoorsJson = jsx:encode(format_doors(Doors)),

    Chests = map_data:get_chests(Area, MapX, MapY),
    ChestsJson = jsx:encode(format_chests(Chests)),

    %% Player position
    TileX = maps:get(tile_x, GameState, 8),
    TileY = maps:get(tile_y, GameState, 6),

    %% Keys owned
    Keys = maps:get(keys, GameState, []),
    KeysJson = jsx:encode(Keys),

    %% Enemies killed on current map
    KilledEnemies = game_state:get_enemies_killed(GameState, Area, {MapX, MapY}),
    KilledJson = jsx:encode(KilledEnemies),

    %% Chests opened
    ChestsOpened = maps:get(chests_opened, GameState, []),
    ChestsOpenedJson = jsx:encode(format_chests_opened(ChestsOpened, Area, MapX, MapY)),

    iolist_to_binary([
        <<"<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>ERHTMX Adventure</title>
    <script src=\"https://unpkg.com/htmx.org@1.9.10\"></script>
    <link rel=\"stylesheet\" href=\"/static/style.css\">
</head>
<body>
    <div id=\"game-container\">
        <!-- Character Info Bar (Top Left) -->
        <div id=\"character-bar\">
            <div class=\"info-box\">
                <div class=\"char-name\">">>, Name, <<"</div>
                <div class=\"char-class\">">>, atom_to_binary(Class, utf8), <<"</div>
                <div class=\"hp-bar-container\">
                    <div class=\"hp-bar\" style=\"width: ">>,
                        integer_to_binary(round(HP / MaxHP * 100)), <<"%\"></div>
                    <div class=\"hp-text\">">>, integer_to_binary(HP), <<" / ">>, integer_to_binary(MaxHP), <<"</div>
                </div>
            </div>
        </div>

        <!-- Location Display (Top Right) -->
        <div id=\"location-bar\">
            <div class=\"info-box\">
                <div class=\"area-name\">">>, format_area_name(Area), <<"</div>
                <div class=\"map-pos\">Map: ">>, integer_to_binary(MapX + 1), <<", ">>, integer_to_binary(MapY + 1), <<"</div>
            </div>
        </div>

        <!-- Game Canvas -->
        <canvas id=\"game-canvas\" width=\"640\" height=\"480\"></canvas>

        <!-- Inventory Bar (Bottom) -->
        <div id=\"inventory-bar\">
            <div class=\"inventory-container\">">>,
                render_inventory_slots(Inventory),
            <<"</div>
        </div>

        <!-- Message Display -->
        <div id=\"message-display\"></div>
    </div>

    <!-- Game State (passed to JavaScript) -->
    <script>
        window.GAME_STATE = {
            playerName: \"">>, Name, <<"\",
            playerClass: \"">>, atom_to_binary(Class, utf8), <<"\",
            hp: ">>, integer_to_binary(HP), <<",
            maxHp: ">>, integer_to_binary(MaxHP), <<",
            area: \"">>, atom_to_binary(Area, utf8), <<"\",
            mapX: ">>, integer_to_binary(MapX), <<",
            mapY: ">>, integer_to_binary(MapY), <<",
            tileX: ">>, integer_to_binary(TileX), <<",
            tileY: ">>, integer_to_binary(TileY), <<",
            inventory: ">>, InventoryJson, <<",
            keys: ">>, KeysJson, <<",
            mapTiles: ">>, MapJson, <<",
            enemies: ">>, EnemiesJson, <<",
            doors: ">>, DoorsJson, <<",
            chests: ">>, ChestsJson, <<",
            killedEnemies: ">>, KilledJson, <<",
            openedChests: ">>, ChestsOpenedJson, <<"
        };
    </script>
    <script src=\"/static/game.js\"></script>
</body>
</html>">>
    ]).

%%--------------------------------------------------------------------
%% @doc Formats area name for display.
%% @end
%%--------------------------------------------------------------------
format_area_name(training_grounds) -> <<"Training Grounds">>;
format_area_name(castle) -> <<"The Castle">>;
format_area_name(dungeon) -> <<"The Dungeon">>.

%%--------------------------------------------------------------------
%% @doc Renders inventory slot HTML.
%% @end
%%--------------------------------------------------------------------
render_inventory_slots(Inventory) ->
    %% 8 inventory slots
    Slots = lists:seq(1, 8),
    [render_slot(I, lists:nth(I, Inventory ++ lists:duplicate(8, <<"">>))) || I <- Slots].

render_slot(Index, Item) ->
    ItemClass = if Item == <<"">> -> <<"empty">>; true -> <<"filled">> end,
    ItemName = format_item_name(Item),
    iolist_to_binary([
        <<"<div class=\"inventory-slot ">>, ItemClass, <<"\" data-slot=\"">>,
        integer_to_binary(Index), <<"\">">>,
        <<"<span class=\"item-icon\">">>, get_item_icon(Item), <<"</span>">>,
        <<"<span class=\"item-name\">">>, ItemName, <<"</span>">>,
        <<"</div>">>
    ]).

format_item_name(<<"">>) -> <<"">>;
format_item_name(<<"sword">>) -> <<"Sword">>;
format_item_name(<<"dagger">>) -> <<"Dagger">>;
format_item_name(<<"spear">>) -> <<"Spear">>;
format_item_name(<<"bow">>) -> <<"Bow">>;
format_item_name(<<"staff_fire">>) -> <<"Fire Staff">>;
format_item_name(<<"staff_lightning">>) -> <<"Lightning Staff">>;
format_item_name(<<"staff_ice">>) -> <<"Ice Staff">>;
format_item_name(<<"health_potion">>) -> <<"Health Potion">>;
format_item_name(Other) -> Other.

get_item_icon(<<"">>) -> <<"">>;
get_item_icon(<<"sword">>) -> <<"&#9876;">>;  % Crossed swords
get_item_icon(<<"dagger">>) -> <<"&#128481;">>; % Dagger
get_item_icon(<<"spear">>) -> <<"&#129717;">>; % Long arrow (spear-like)
get_item_icon(<<"bow">>) -> <<"&#127993;">>; % Bow and arrow
get_item_icon(<<"staff_fire">>) -> <<"&#128293;">>; % Fire
get_item_icon(<<"staff_lightning">>) -> <<"&#9889;">>; % Lightning
get_item_icon(<<"staff_ice">>) -> <<"&#10052;">>; % Snowflake
get_item_icon(<<"health_potion">>) -> <<"&#129346;">>; % Potion
get_item_icon(_) -> <<"&#10067;">>. % Question mark

%%--------------------------------------------------------------------
%% @doc Formats enemies for JSON.
%% @end
%%--------------------------------------------------------------------
format_enemies(Enemies) ->
    [#{
        id => Id,
        type => atom_to_binary(Type, utf8),
        x => X,
        y => Y,
        hp => HP
    } || {Id, Type, X, Y, HP} <- Enemies].

%%--------------------------------------------------------------------
%% @doc Formats doors for JSON.
%% @end
%%--------------------------------------------------------------------
format_doors(Doors) ->
    [#{
        direction => atom_to_binary(Dir, utf8),
        targetArea => atom_to_binary(TargetArea, utf8),
        targetX => TargetX,
        targetY => TargetY,
        keyRequired => case Key of none -> null; _ -> Key end
    } || {Dir, TargetArea, TargetX, TargetY, Key} <- Doors].

%%--------------------------------------------------------------------
%% @doc Formats chests for JSON.
%% @end
%%--------------------------------------------------------------------
format_chests(Chests) ->
    [#{
        id => Id,
        x => X,
        y => Y,
        contents => format_contents(Contents)
    } || {Id, X, Y, Contents} <- Chests].

format_contents({key, KeyId}) -> #{type => <<"key">>, id => KeyId};
format_contents({item, ItemName}) -> #{type => <<"item">>, id => ItemName}.

%%--------------------------------------------------------------------
%% @doc Formats opened chests for current map.
%% @end
%%--------------------------------------------------------------------
format_chests_opened(ChestsOpened, Area, MapX, MapY) ->
    [ChestId || {A, MX, MY, ChestId} <- ChestsOpened,
                A == Area, MX == MapX, MY == MapY].
