%%%-------------------------------------------------------------------
%%% @doc game_api_handler
%%% HTTP handler for the game state API.
%%%
%%% This handler provides endpoints for:
%%% - GET: Retrieve current game state
%%% - POST: Update game state (position, HP, inventory, etc.)
%%%
%%% The state is stored in cookies, so the server remains stateless.
%%% The JavaScript game engine calls this API to persist changes.
%%% @end
%%%-------------------------------------------------------------------
-module(game_api_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handles API requests for game state.
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            handle_get(Req0, State);
        <<"POST">> ->
            handle_post(Req0, State);
        _ ->
            Req = cowboy_req:reply(405, #{
                <<"content-type">> => <<"application/json">>
            }, <<"{\"error\": \"Method not allowed\"}">>, Req0),
            {ok, Req, State}
    end.

%%--------------------------------------------------------------------
%% @doc Handles GET requests - returns current game state.
%% @end
%%--------------------------------------------------------------------
handle_get(Req0, State) ->
    GameState = get_game_state(Req0),
    Json = game_state:to_json(GameState),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Json, Req0),
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc Handles POST requests - updates game state.
%%
%% Expected JSON body:
%% {
%%   "action": "update" | "take_damage" | "heal" | "add_item" |
%%             "add_key" | "open_chest" | "kill_enemy" | "move_map",
%%   ...action-specific fields...
%% }
%% @end
%%--------------------------------------------------------------------
handle_post(Req0, State) ->
    %% Get current state from cookie
    GameState = get_game_state(Req0),

    %% Read request body
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    %% Parse JSON
    Result = try
        Data = jsx:decode(Body, [return_maps]),
        process_action(Data, GameState)
    catch
        _:Error ->
            io:format("API Error: ~p~n", [Error]),
            {error, <<"Invalid request">>}
    end,

    BasePath = game_handler:get_base_path(),
    CookiePath = case BasePath of
        <<>> -> <<"/">>;
        _ -> BasePath
    end,

    case Result of
        {ok, NewState} ->
            %% Encode and set cookie
            JsonState = game_state:to_json(NewState),
            EncodedState = uri_string:quote(JsonState),

            %% Set cookie using proper cowboy function
            Req2 = cowboy_req:set_resp_cookie(
                <<"game_state">>,
                EncodedState,
                Req1,
                #{path => CookiePath, max_age => 31536000}
            ),

            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, JsonState, Req2),
            {ok, Req, State};

        {error, Message} ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => Message}), Req1),
            {ok, Req, State}
    end.

%%--------------------------------------------------------------------
%% @doc Processes different action types.
%% @end
%%--------------------------------------------------------------------
process_action(#{<<"action">> := <<"update">>} = Data, GameState) ->
    %% General update - merge provided fields
    Updates = maps:without([<<"action">>], Data),
    NewState = maps:merge(GameState, convert_update(Updates)),
    {ok, NewState};

process_action(#{<<"action">> := <<"take_damage">>, <<"damage">> := Damage}, GameState) ->
    {NewState, _Status} = game_state:take_damage(GameState, Damage),
    {ok, NewState};

process_action(#{<<"action">> := <<"heal">>, <<"amount">> := Amount}, GameState) ->
    NewState = game_state:heal(GameState, Amount),
    {ok, NewState};

process_action(#{<<"action">> := <<"add_item">>, <<"item">> := Item}, GameState) ->
    NewState = game_state:add_item(GameState, Item),
    {ok, NewState};

process_action(#{<<"action">> := <<"remove_item">>, <<"item">> := Item}, GameState) ->
    NewState = game_state:remove_item(GameState, Item),
    {ok, NewState};

process_action(#{<<"action">> := <<"use_potion">>}, GameState) ->
    %% Use a health potion: heal and remove from inventory
    NewState = game_state:heal(GameState, 30),
    NewState2 = game_state:remove_item(NewState, <<"health_potion">>),
    {ok, NewState2};

process_action(#{<<"action">> := <<"add_key">>, <<"key">> := KeyId}, GameState) ->
    NewState = game_state:add_key(GameState, KeyId),
    {ok, NewState};

process_action(#{<<"action">> := <<"open_chest">>, <<"chestId">> := ChestId}, GameState) ->
    Area = maps:get(area, GameState),
    MapX = maps:get(map_x, GameState),
    MapY = maps:get(map_y, GameState),
    NewState = game_state:mark_chest_opened(GameState, {Area, MapX, MapY}, ChestId),
    {ok, NewState};

process_action(#{<<"action">> := <<"kill_enemy">>, <<"enemyId">> := EnemyId}, GameState) ->
    Area = maps:get(area, GameState),
    MapX = maps:get(map_x, GameState),
    MapY = maps:get(map_y, GameState),
    NewState = game_state:mark_enemy_killed(GameState, {Area, MapX, MapY}, EnemyId, true),
    {ok, NewState};

process_action(#{<<"action">> := <<"move_map">>} = Data, GameState) ->
    %% Player moved to a new map
    AreaBin = maps:get(<<"area">>, Data),
    MapX = maps:get(<<"mapX">>, Data),
    MapY = maps:get(<<"mapY">>, Data),
    TileX = maps:get(<<"tileX">>, Data),
    TileY = maps:get(<<"tileY">>, Data),

    %% Convert area to atom
    Area = case AreaBin of
        <<"training_grounds">> -> training_grounds;
        <<"castle">> -> castle;
        <<"dungeon">> -> dungeon;
        _ -> maps:get(area, GameState)
    end,

    NewState = game_state:update_position(GameState, Area, {MapX, MapY}, {TileX, TileY}),
    {ok, NewState};

process_action(#{<<"action">> := <<"defeat_dragon">>}, GameState) ->
    NewState = GameState#{dragon_defeated => true},
    {ok, NewState};

process_action(#{<<"action">> := <<"reset">>}, _GameState) ->
    %% Reset to uninitialized state (forces character creation)
    {ok, game_state:default_state()};

process_action(#{<<"action">> := <<"save_floor_entry_hp">>}, GameState) ->
    %% Save current HP as floor entry HP (called when entering a new floor)
    CurrentHp = maps:get(hp, GameState, 100),
    NewState = GameState#{floor_entry_hp => CurrentHp},
    {ok, NewState};

process_action(#{<<"action">> := <<"restore_floor_entry_hp">>}, GameState) ->
    %% Restore HP to floor entry HP (called on "Try Again")
    FloorEntryHp = maps:get(floor_entry_hp, GameState, maps:get(max_hp, GameState, 100)),
    NewState = GameState#{hp => FloorEntryHp},
    {ok, NewState};

process_action(_, _) ->
    {error, <<"Unknown action">>}.

%%--------------------------------------------------------------------
%% @doc Gets game state from request cookies.
%% @end
%%--------------------------------------------------------------------
get_game_state(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(<<"game_state">>, 1, Cookies) of
        {_, EncodedState} ->
            try
                Decoded = uri_string:percent_decode(EncodedState),
                case game_state:from_json(Decoded) of
                    {error, _} -> game_state:default_state();
                    State -> State
                end
            catch
                _:_ -> game_state:default_state()
            end;
        false ->
            game_state:default_state()
    end.

%%--------------------------------------------------------------------
%% @doc Converts update data from JSON format to internal format.
%% @end
%%--------------------------------------------------------------------
convert_update(Updates) ->
    maps:fold(fun(K, V, Acc) ->
        Key = binary_to_atom(K, utf8),
        Acc#{Key => V}
    end, #{}, Updates).
