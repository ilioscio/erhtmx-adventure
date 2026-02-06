%%%-------------------------------------------------------------------
%%% @doc map_data
%%% Defines the map layouts for all areas in the game.
%%%
%%% The game world is divided into 3 areas:
%%% - training_grounds: Safe starting area (16 maps in 4x4 grid)
%%% - castle: Medium difficulty (16 maps in 4x4 grid)
%%% - dungeon: Hard area with the dragon boss (16 maps in 4x4 grid)
%%%
%%% Each map is a 16x12 tile grid (matching a 640x480 canvas at 40px tiles).
%%% Maps contain:
%%% - Tiles (floor, wall, water, etc.)
%%% - Enemies (with spawn positions)
%%% - Doors (connections to adjacent maps or areas)
%%% - Chests (containing keys or items)
%%%
%%% Tile types:
%%% - 0: Floor (walkable)
%%% - 1: Wall (blocking)
%%% - 2: Water (blocking)
%%% - 3: Door (triggers map transition)
%%% - 4: Locked door (needs key)
%%% - 5: Chest
%%% - 6: Stairs up (area transition)
%%% - 7: Stairs down (area transition)
%%% @end
%%%-------------------------------------------------------------------
-module(map_data).

-export([
    get_map/3,
    get_enemies/3,
    get_doors/3,
    get_chests/3,
    get_area_transition/3,
    all_areas/0,
    map_width/0,
    map_height/0,
    tile_size/0
]).

%% Map dimensions (in tiles)
-define(MAP_WIDTH, 16).
-define(MAP_HEIGHT, 12).
-define(TILE_SIZE, 40).

map_width() -> ?MAP_WIDTH.
map_height() -> ?MAP_HEIGHT.
tile_size() -> ?TILE_SIZE.

all_areas() -> [training_grounds, castle, dungeon].

%%--------------------------------------------------------------------
%% @doc Gets the tile layout for a specific map.
%% Returns a list of rows, where each row is a list of tile types.
%% @end
%%--------------------------------------------------------------------
get_map(Area, X, Y) when X >= 0, X =< 3, Y >= 0, Y =< 3 ->
    generate_map(Area, X, Y).

%%--------------------------------------------------------------------
%% @doc Gets enemy spawn data for a specific map.
%% Returns list of {EnemyId, Type, X, Y, HP} tuples.
%% @end
%%--------------------------------------------------------------------
get_enemies(Area, X, Y) ->
    generate_enemies(Area, X, Y).

%%--------------------------------------------------------------------
%% @doc Gets door data for a specific map.
%% Returns list of {Direction, TargetArea, TargetX, TargetY, KeyRequired} tuples.
%% Direction is north, south, east, or west.
%% KeyRequired is 'none' or a key ID binary.
%% @end
%%--------------------------------------------------------------------
get_doors(Area, X, Y) ->
    generate_doors(Area, X, Y).

%%--------------------------------------------------------------------
%% @doc Gets chest data for a specific map.
%% Returns list of {ChestId, TileX, TileY, Contents} tuples.
%% Contents is either {key, KeyId} or {item, ItemName}.
%% @end
%%--------------------------------------------------------------------
get_chests(Area, X, Y) ->
    generate_chests(Area, X, Y).

%%--------------------------------------------------------------------
%% @doc Checks for area transitions (stairs) on a map.
%% Returns {stairs_up, TargetArea} or {stairs_down, TargetArea} or none.
%% @end
%%--------------------------------------------------------------------
get_area_transition(Area, X, Y) ->
    case {Area, X, Y} of
        %% Training grounds (2,3) has stairs to castle - bottom center-left
        {training_grounds, 2, 3} -> {stairs_down, castle, 1, 0};
        %% Castle (3,2) has stairs to dungeon - right side
        {castle, 3, 2} -> {stairs_down, dungeon, 0, 1};
        %% Castle (1,0) has stairs back to training grounds - different location
        {castle, 1, 0} -> {stairs_up, training_grounds, 2, 3};
        %% Dungeon (0,1) has stairs back to castle
        {dungeon, 0, 1} -> {stairs_up, castle, 3, 2};
        _ -> none
    end.

%%--------------------------------------------------------------------
%% Internal: Generate map layout based on area and position
%%--------------------------------------------------------------------
generate_map(Area, X, Y) ->
    %% Create base floor map
    BaseMap = create_base_map(),

    %% Add walls based on area style
    WithWalls = add_area_walls(BaseMap, Area, X, Y),

    %% Add doors on edges where adjacent maps exist
    WithDoors = add_door_tiles(WithWalls, Area, X, Y),

    %% Add special features
    add_special_features(WithDoors, Area, X, Y).

%% Create a basic floor map with walls around the border
create_base_map() ->
    [create_row(Y) || Y <- lists:seq(0, ?MAP_HEIGHT - 1)].

create_row(Y) ->
    [tile_at(X, Y) || X <- lists:seq(0, ?MAP_WIDTH - 1)].

tile_at(X, Y) ->
    %% Border walls
    if
        Y == 0 -> 1;
        Y == ?MAP_HEIGHT - 1 -> 1;
        X == 0 -> 1;
        X == ?MAP_WIDTH - 1 -> 1;
        true -> 0
    end.

%% Add interior walls based on area style
add_area_walls(Map, Area, X, Y) ->
    Seed = erlang:phash2({Area, X, Y}),
    rand:seed(exsplus, {Seed, Seed * 2, Seed * 3}),

    case Area of
        training_grounds ->
            %% Simple layouts for training
            add_training_walls(Map, X, Y);
        castle ->
            %% Room-like layouts
            add_castle_walls(Map, X, Y);
        dungeon ->
            %% Maze-like layouts
            add_dungeon_walls(Map, X, Y)
    end.

add_training_walls(Map, X, Y) ->
    %% Training grounds have minimal obstacles
    case {X, Y} of
        {0, 0} ->
            %% Starting room - completely open
            Map;
        {1, 0} ->
            %% Add a few pillars
            add_pillars(Map, [{4, 4}, {11, 4}, {4, 7}, {11, 7}]);
        _ ->
            %% Random simple obstacles, avoiding spawn points
            NumPillars = rand:uniform(3),
            Positions = generate_safe_positions(NumPillars),
            add_pillars(Map, Positions)
    end.

%% Generate pillar positions that don't block spawn points
generate_safe_positions(Num) ->
    %% Safe spawn zones to avoid:
    %% - Center: (7,5) to (9,7) - general spawn area
    %% - North door entry: around (7, 10)
    %% - South door entry: around (7, 1)
    %% - West door entry: around (14, 5)
    %% - East door entry: around (1, 5)
    SafeZones = [
        {6, 4, 10, 8},   % Center spawn area
        {6, 9, 9, 11},   % North door entry
        {6, 1, 9, 3},    % South door entry
        {1, 4, 3, 7},    % East door entry
        {12, 4, 15, 7}   % West door entry
    ],
    generate_safe_positions(Num, SafeZones, []).

generate_safe_positions(0, _, Acc) ->
    Acc;
generate_safe_positions(Num, SafeZones, Acc) ->
    X = rand:uniform(12) + 1,
    Y = rand:uniform(8) + 1,
    case is_in_safe_zone(X, Y, SafeZones) of
        true ->
            %% Try again
            generate_safe_positions(Num, SafeZones, Acc);
        false ->
            generate_safe_positions(Num - 1, SafeZones, [{X, Y} | Acc])
    end.

is_in_safe_zone(X, Y, SafeZones) ->
    lists:any(fun({X1, Y1, X2, Y2}) ->
        X >= X1 andalso X =< X2 andalso Y >= Y1 andalso Y =< Y2
    end, SafeZones).

add_castle_walls(Map, _X, _Y) ->
    %% Castle has room-like structures
    %% Add some internal walls - positioned away from spawn zones
    Walls = [
        {4, 3}, {4, 4},
        {11, 7}, {11, 8}
    ],
    add_pillars(Map, Walls).

add_dungeon_walls(Map, _X, _Y) ->
    %% Dungeon has more complex layouts - positioned away from spawn zones
    Walls = [
        {3, 3}, {3, 4},
        {12, 3}, {12, 4},
        {3, 8}, {3, 9},
        {12, 8}, {12, 9}
    ],
    add_pillars(Map, Walls).

add_pillars(Map, Positions) ->
    lists:foldl(fun({PX, PY}, AccMap) ->
        set_tile(AccMap, PX, PY, 1)
    end, Map, Positions).

%% Add door tiles at map edges
add_door_tiles(Map, Area, X, Y) ->
    Doors = generate_doors(Area, X, Y),
    lists:foldl(fun({Dir, _, _, _, KeyReq}, AccMap) ->
        TileType = case KeyReq of
            none -> 3;
            _ -> 4
        end,
        case Dir of
            north -> set_tile(AccMap, 7, 0, TileType);
            south -> set_tile(AccMap, 7, ?MAP_HEIGHT - 1, TileType);
            west -> set_tile(AccMap, 0, 5, TileType);
            east -> set_tile(AccMap, ?MAP_WIDTH - 1, 5, TileType)
        end
    end, Map, Doors).

%% Add special features (stairs, chests)
add_special_features(Map, Area, X, Y) ->
    %% Add stairs if this map has area transition
    Map1 = case get_area_transition(Area, X, Y) of
        {stairs_down, _, _, _} -> set_tile(Map, 8, 6, 7);
        {stairs_up, _, _, _} -> set_tile(Map, 8, 6, 6);
        none -> Map
    end,

    %% Add chests
    Chests = generate_chests(Area, X, Y),
    lists:foldl(fun({_Id, CX, CY, _Contents}, AccMap) ->
        set_tile(AccMap, CX, CY, 5)
    end, Map1, Chests).

%% Helper to set a tile in the map
set_tile(Map, X, Y, Tile) when X >= 0, X < ?MAP_WIDTH, Y >= 0, Y < ?MAP_HEIGHT ->
    Row = lists:nth(Y + 1, Map),
    NewRow = lists:sublist(Row, X) ++ [Tile] ++ lists:nthtail(X + 1, Row),
    lists:sublist(Map, Y) ++ [NewRow] ++ lists:nthtail(Y + 1, Map);
set_tile(Map, _, _, _) ->
    Map.

%%--------------------------------------------------------------------
%% Generate enemies based on area difficulty
%%--------------------------------------------------------------------
generate_enemies(Area, X, Y) ->
    %% Use deterministic seed for consistent enemy placement
    Seed = erlang:phash2({Area, X, Y, enemies}),
    rand:seed(exsplus, {Seed, Seed * 2, Seed * 3}),

    %% Special cases: starting room and dragon's lair
    case {Area, X, Y} of
        {training_grounds, 0, 0} ->
            %% Starting room - no enemies
            [];
        {dungeon, 3, 3} ->
            %% Dragon's lair - only the dragon boss
            [{<<"dragon_boss">>, dragon, 8, 6, 300}];
        _ ->
            %% Get the map to check for valid spawn positions
            MapTiles = generate_map(Area, X, Y),
            NumEnemies = get_enemy_count(Area),
            [create_enemy(Area, X, Y, I, MapTiles) || I <- lists:seq(1, NumEnemies)]
    end.

get_enemy_count(training_grounds) -> rand:uniform(2) + 1;
get_enemy_count(castle) -> rand:uniform(3) + 2;
get_enemy_count(dungeon) -> rand:uniform(4) + 3.

create_enemy(Area, MapX, MapY, Index, MapTiles) ->
    %% Enemy ID is unique per map
    Id = list_to_binary(io_lib:format("enemy_~s_~p_~p_~p",
        [Area, MapX, MapY, Index])),

    %% Find a valid position (not a wall, water, or special tile)
    {EX, EY} = find_valid_enemy_position(MapTiles, 20),

    %% Enemy type and HP based on area
    {Type, HP} = case Area of
        training_grounds ->
            {slime, 20};
        castle ->
            case rand:uniform(2) of
                1 -> {skeleton, 40};
                2 -> {bat, 25}
            end;
        dungeon ->
            case rand:uniform(3) of
                1 -> {demon, 60};
                2 -> {ghost, 45};
                3 -> {spider, 35}
            end
    end,

    {Id, Type, EX, EY, HP}.

%% Find a valid position for enemy spawn (floor tiles only)
find_valid_enemy_position(MapTiles, AttemptsLeft) when AttemptsLeft > 0 ->
    %% Try random position avoiding edges
    EX = rand:uniform(10) + 2,
    EY = rand:uniform(6) + 2,
    %% Check if position is a floor tile (type 0)
    case get_tile(MapTiles, EX, EY) of
        0 -> {EX, EY};  % Floor tile, valid position
        _ -> find_valid_enemy_position(MapTiles, AttemptsLeft - 1)
    end;
find_valid_enemy_position(_, _) ->
    %% Fallback to center if no valid position found
    {8, 6}.

%% Get tile at position
get_tile(MapTiles, X, Y) when X >= 0, X < ?MAP_WIDTH, Y >= 0, Y < ?MAP_HEIGHT ->
    Row = lists:nth(Y + 1, MapTiles),
    lists:nth(X + 1, Row);
get_tile(_, _, _) ->
    1.  % Out of bounds = wall

%%--------------------------------------------------------------------
%% Generate doors connecting maps
%%--------------------------------------------------------------------
generate_doors(Area, X, Y) ->
    Doors = [],

    %% North door (if Y > 0)
    D1 = if Y > 0 -> [{north, Area, X, Y - 1, none}]; true -> [] end,

    %% South door (if Y < 3)
    D2 = if Y < 3 -> [{south, Area, X, Y + 1, none}]; true -> [] end,

    %% West door (if X > 0)
    D3 = if X > 0 -> [{west, Area, X - 1, Y, none}]; true -> [] end,

    %% East door (if X < 3)
    D4 = if X < 3 -> [{east, Area, X + 1, Y, none}]; true -> [] end,

    %% Add locked doors for special locations
    LockedDoors = get_locked_doors(Area, X, Y),

    Doors ++ D1 ++ D2 ++ D3 ++ D4 ++ LockedDoors.

get_locked_doors(castle, 2, 2) ->
    %% Castle inner room needs a key
    [{east, castle, 3, 2, <<"castle_key">>}];
get_locked_doors(dungeon, 2, 3) ->
    %% Dragon's lair needs final key (east leads to dragon)
    [{east, dungeon, 3, 3, <<"dragon_key">>}];
get_locked_doors(_, _, _) ->
    [].

%%--------------------------------------------------------------------
%% Generate chests
%%--------------------------------------------------------------------
generate_chests(Area, X, Y) ->
    case {Area, X, Y} of
        %% Training grounds key chest
        {training_grounds, 2, 1} ->
            [{<<"chest_1">>, 8, 6, {item, <<"health_potion">>}}];

        %% Castle key location
        {castle, 1, 2} ->
            [{<<"chest_castle_key">>, 8, 6, {key, <<"castle_key">>}}];

        %% Dungeon key location
        {dungeon, 1, 3} ->
            [{<<"chest_dragon_key">>, 8, 6, {key, <<"dragon_key">>}}];

        %% Some random item chests
        {training_grounds, 3, 2} ->
            [{<<"chest_2">>, 10, 4, {item, <<"health_potion">>}}];

        {castle, 2, 1} ->
            [{<<"chest_3">>, 5, 7, {item, <<"staff_lightning">>}}];

        {dungeon, 0, 2} ->
            [{<<"chest_4">>, 12, 8, {item, <<"staff_ice">>}}];

        _ ->
            []
    end.
