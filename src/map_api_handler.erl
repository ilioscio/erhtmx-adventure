%%%-------------------------------------------------------------------
%%% @doc map_api_handler
%%% HTTP handler for map data API.
%%%
%%% Returns map data including tiles, enemies, doors, and chests
%%% for a specific area and position.
%%%
%%% URL format: /api/map/:area/:x/:y
%%% @end
%%%-------------------------------------------------------------------
-module(map_api_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handles GET requests for map data.
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    %% Extract path parameters
    AreaBin = cowboy_req:binding(area, Req0),
    XBin = cowboy_req:binding(x, Req0),
    YBin = cowboy_req:binding(y, Req0),

    %% Convert parameters
    Area = case AreaBin of
        <<"training_grounds">> -> training_grounds;
        <<"castle">> -> castle;
        <<"dungeon">> -> dungeon;
        _ -> training_grounds
    end,

    X = try binary_to_integer(XBin) catch _:_ -> 0 end,
    Y = try binary_to_integer(YBin) catch _:_ -> 0 end,

    %% Validate coordinates
    ValidX = max(0, min(3, X)),
    ValidY = max(0, min(3, Y)),

    %% Get map data
    MapTiles = map_data:get_map(Area, ValidX, ValidY),
    Enemies = map_data:get_enemies(Area, ValidX, ValidY),
    Doors = map_data:get_doors(Area, ValidX, ValidY),
    Chests = map_data:get_chests(Area, ValidX, ValidY),
    Transition = map_data:get_area_transition(Area, ValidX, ValidY),

    %% Build response
    Response = #{
        area => atom_to_binary(Area, utf8),
        x => ValidX,
        y => ValidY,
        tiles => MapTiles,
        enemies => format_enemies(Enemies),
        doors => format_doors(Doors),
        chests => format_chests(Chests),
        transition => format_transition(Transition)
    },

    Json = jsx:encode(Response),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Json, Req0),
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc Formats enemies for JSON response.
%% @end
%%--------------------------------------------------------------------
format_enemies(Enemies) ->
    [#{
        id => Id,
        type => atom_to_binary(Type, utf8),
        x => X,
        y => Y,
        hp => HP,
        maxHp => HP
    } || {Id, Type, X, Y, HP} <- Enemies].

%%--------------------------------------------------------------------
%% @doc Formats doors for JSON response.
%% @end
%%--------------------------------------------------------------------
format_doors(Doors) ->
    [#{
        direction => atom_to_binary(Dir, utf8),
        targetArea => atom_to_binary(TargetArea, utf8),
        targetX => TargetX,
        targetY => TargetY,
        keyRequired => case KeyReq of none -> null; _ -> KeyReq end
    } || {Dir, TargetArea, TargetX, TargetY, KeyReq} <- Doors].

%%--------------------------------------------------------------------
%% @doc Formats chests for JSON response.
%% @end
%%--------------------------------------------------------------------
format_chests(Chests) ->
    [#{
        id => Id,
        x => X,
        y => Y,
        contents => format_contents(Contents)
    } || {Id, X, Y, Contents} <- Chests].

format_contents({key, KeyId}) ->
    #{type => <<"key">>, id => KeyId};
format_contents({item, ItemName}) ->
    #{type => <<"item">>, id => ItemName}.

%%--------------------------------------------------------------------
%% @doc Formats area transition for JSON response.
%% @end
%%--------------------------------------------------------------------
format_transition(none) ->
    null;
format_transition({Type, TargetArea, TargetX, TargetY}) ->
    #{
        type => atom_to_binary(Type, utf8),
        targetArea => atom_to_binary(TargetArea, utf8),
        targetX => TargetX,
        targetY => TargetY
    }.
