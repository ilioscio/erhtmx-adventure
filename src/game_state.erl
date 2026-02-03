%%%-------------------------------------------------------------------
%%% @doc game_state
%%% Defines the game state data structures and provides functions
%%% for creating, modifying, and serializing game state.
%%%
%%% Game state is stored in browser cookies as JSON, allowing the
%%% server to remain stateless. This module handles:
%%% - Player creation with different classes
%%% - Inventory management
%%% - Position tracking (area, map x/y, tile x/y)
%%% - HP and combat stats
%%% - Progression (keys collected, chests opened, enemies killed)
%%%
%%% Classes:
%%% - wizard: Staff with 3 spell types (fire, lightning, ice)
%%% - archer: Bow with fast projectiles
%%% - warrior_sword: Sword with 90-degree arc attack
%%% - warrior_dagger: Dagger with high-speed poke
%%% - warrior_spear: Spear with medium range poke
%%% @end
%%%-------------------------------------------------------------------
-module(game_state).

-export([
    new_player/2,
    to_json/1,
    from_json/1,
    update_position/4,
    take_damage/2,
    heal/2,
    add_item/2,
    remove_item/2,
    has_item/2,
    add_key/2,
    has_key/2,
    mark_chest_opened/3,
    is_chest_opened/3,
    mark_enemy_killed/4,
    get_enemies_killed/3,
    get_starting_weapon/1,
    get_max_hp/1,
    default_state/0
]).

%%--------------------------------------------------------------------
%% @doc Creates a new player with the given name and class.
%% Each class starts with different stats and weapons.
%%
%% Returns a map containing all player state:
%% - name: Player's chosen name
%% - class: One of wizard, archer, warrior_sword, etc.
%% - hp: Current hit points
%% - max_hp: Maximum hit points
%% - area: Current area (training_grounds, castle, dungeon)
%% - map_x, map_y: Position in the 4x4 grid of maps (0-3)
%% - tile_x, tile_y: Position on current map tile grid
%% - inventory: List of item names
%% - keys: List of key IDs collected
%% - chests_opened: List of {Area, MapX, MapY, ChestId} tuples
%% - enemies_killed: Map of killed enemy IDs per map
%% @end
%%--------------------------------------------------------------------
new_player(Name, Class) when is_binary(Name), is_atom(Class) ->
    MaxHp = get_max_hp(Class),
    StartingWeapon = get_starting_weapon(Class),
    #{
        name => Name,
        class => Class,
        hp => MaxHp,
        max_hp => MaxHp,
        %% Start in training grounds at map (0,0)
        area => training_grounds,
        map_x => 0,
        map_y => 0,
        %% Start in center of map
        tile_x => 8,
        tile_y => 6,
        %% Inventory starts with class weapon
        inventory => [StartingWeapon],
        %% Collected keys (for locked doors)
        keys => [],
        %% Opened chests (so they stay open)
        chests_opened => [],
        %% Killed enemies per map (for respawn tracking)
        enemies_killed => #{},
        %% Game progression flags
        dragon_defeated => false
    }.

%%--------------------------------------------------------------------
%% @doc Returns the starting weapon for each class.
%% @end
%%--------------------------------------------------------------------
get_starting_weapon(wizard) -> <<"staff_fire">>;
get_starting_weapon(archer) -> <<"bow">>;
get_starting_weapon(warrior_sword) -> <<"sword">>;
get_starting_weapon(warrior_dagger) -> <<"dagger">>;
get_starting_weapon(warrior_spear) -> <<"spear">>.

%%--------------------------------------------------------------------
%% @doc Returns the maximum HP for each class.
%% Warriors have more HP, wizard has less.
%% @end
%%--------------------------------------------------------------------
get_max_hp(wizard) -> 80;
get_max_hp(archer) -> 100;
get_max_hp(warrior_sword) -> 120;
get_max_hp(warrior_dagger) -> 100;
get_max_hp(warrior_spear) -> 110.

%%--------------------------------------------------------------------
%% @doc Converts game state map to JSON binary.
%% Used for storing state in cookies.
%% @end
%%--------------------------------------------------------------------
to_json(State) when is_map(State) ->
    %% Convert atoms to binaries for JSON compatibility
    JsonState = prepare_for_json(State),
    jsx:encode(JsonState).

%%--------------------------------------------------------------------
%% @doc Parses JSON binary into game state map.
%% Used for reading state from cookies.
%% @end
%%--------------------------------------------------------------------
from_json(JsonBin) when is_binary(JsonBin) ->
    try
        Decoded = jsx:decode(JsonBin, [return_maps]),
        restore_from_json(Decoded)
    catch
        _:_ -> {error, invalid_json}
    end.

%%--------------------------------------------------------------------
%% @doc Updates the player's position in the game world.
%% @end
%%--------------------------------------------------------------------
update_position(State, Area, {MapX, MapY}, {TileX, TileY}) ->
    State#{
        area => Area,
        map_x => MapX,
        map_y => MapY,
        tile_x => TileX,
        tile_y => TileY
    }.

%%--------------------------------------------------------------------
%% @doc Reduces player HP by Damage amount. Returns {NewState, alive|dead}.
%% @end
%%--------------------------------------------------------------------
take_damage(State, Damage) ->
    CurrentHp = maps:get(hp, State),
    NewHp = max(0, CurrentHp - Damage),
    NewState = State#{hp => NewHp},
    Status = if NewHp > 0 -> alive; true -> dead end,
    {NewState, Status}.

%%--------------------------------------------------------------------
%% @doc Heals player by Amount, capped at max_hp.
%% @end
%%--------------------------------------------------------------------
heal(State, Amount) ->
    CurrentHp = maps:get(hp, State),
    MaxHp = maps:get(max_hp, State),
    NewHp = min(MaxHp, CurrentHp + Amount),
    State#{hp => NewHp}.

%%--------------------------------------------------------------------
%% @doc Adds an item to the player's inventory.
%% @end
%%--------------------------------------------------------------------
add_item(State, Item) when is_binary(Item) ->
    Inventory = maps:get(inventory, State),
    State#{inventory => Inventory ++ [Item]}.

%%--------------------------------------------------------------------
%% @doc Removes an item from the player's inventory.
%% @end
%%--------------------------------------------------------------------
remove_item(State, Item) when is_binary(Item) ->
    Inventory = maps:get(inventory, State),
    State#{inventory => lists:delete(Item, Inventory)}.

%%--------------------------------------------------------------------
%% @doc Checks if player has a specific item.
%% @end
%%--------------------------------------------------------------------
has_item(State, Item) when is_binary(Item) ->
    Inventory = maps:get(inventory, State),
    lists:member(Item, Inventory).

%%--------------------------------------------------------------------
%% @doc Adds a key to the player's key ring.
%% Keys are identified by their ID (e.g., <<"dungeon_key_1">>).
%% @end
%%--------------------------------------------------------------------
add_key(State, KeyId) when is_binary(KeyId) ->
    Keys = maps:get(keys, State),
    case lists:member(KeyId, Keys) of
        true -> State;
        false -> State#{keys => Keys ++ [KeyId]}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if player has a specific key.
%% @end
%%--------------------------------------------------------------------
has_key(State, KeyId) when is_binary(KeyId) ->
    Keys = maps:get(keys, State),
    lists:member(KeyId, Keys).

%%--------------------------------------------------------------------
%% @doc Marks a chest as opened so it stays open on revisit.
%% @end
%%--------------------------------------------------------------------
mark_chest_opened(State, {Area, MapX, MapY}, ChestId) ->
    Opened = maps:get(chests_opened, State),
    Entry = {Area, MapX, MapY, ChestId},
    case lists:member(Entry, Opened) of
        true -> State;
        false -> State#{chests_opened => Opened ++ [Entry]}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a specific chest has been opened.
%% @end
%%--------------------------------------------------------------------
is_chest_opened(State, {Area, MapX, MapY}, ChestId) ->
    Opened = maps:get(chests_opened, State),
    lists:member({Area, MapX, MapY, ChestId}, Opened).

%%--------------------------------------------------------------------
%% @doc Marks an enemy as killed on a specific map.
%% @end
%%--------------------------------------------------------------------
mark_enemy_killed(State, {Area, MapX, MapY}, EnemyId, Killed) ->
    EnemiesKilled = maps:get(enemies_killed, State),
    MapKey = {Area, MapX, MapY},
    MapKilled = maps:get(MapKey, EnemiesKilled, []),
    NewMapKilled = case Killed of
        true -> lists:usort([EnemyId | MapKilled]);
        false -> lists:delete(EnemyId, MapKilled)
    end,
    State#{enemies_killed => EnemiesKilled#{MapKey => NewMapKilled}}.

%%--------------------------------------------------------------------
%% @doc Gets list of killed enemy IDs for a specific map.
%% @end
%%--------------------------------------------------------------------
get_enemies_killed(State, Area, {MapX, MapY}) ->
    EnemiesKilled = maps:get(enemies_killed, State),
    MapKey = {Area, MapX, MapY},
    maps:get(MapKey, EnemiesKilled, []).

%%--------------------------------------------------------------------
%% @doc Returns a default empty state (for new visitors).
%% @end
%%--------------------------------------------------------------------
default_state() ->
    #{
        initialized => false
    }.

%%--------------------------------------------------------------------
%% Internal: Prepare state map for JSON serialization.
%% Converts atoms to binaries and complex types to JSON-friendly formats.
%%--------------------------------------------------------------------
prepare_for_json(State) when is_map(State) ->
    maps:fold(fun(K, V, Acc) ->
        JsonKey = if is_atom(K) -> atom_to_binary(K, utf8); true -> K end,
        JsonVal = prepare_value_for_json(V),
        Acc#{JsonKey => JsonVal}
    end, #{}, State).

prepare_value_for_json(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
prepare_value_for_json(V) when is_list(V) ->
    [prepare_value_for_json(X) || X <- V];
prepare_value_for_json(V) when is_tuple(V) ->
    %% Convert tuples to lists for JSON
    [prepare_value_for_json(X) || X <- tuple_to_list(V)];
prepare_value_for_json(V) when is_map(V) ->
    %% Handle maps with tuple keys (like enemies_killed)
    maps:fold(fun(K, Val, Acc) ->
        JsonKey = if
            is_tuple(K) ->
                %% Convert tuple key to string like "training_grounds_0_1"
                Parts = [prepare_value_for_json(X) || X <- tuple_to_list(K)],
                iolist_to_binary(lists:join(<<"_">>, Parts));
            is_atom(K) ->
                atom_to_binary(K, utf8);
            true ->
                K
        end,
        Acc#{JsonKey => prepare_value_for_json(Val)}
    end, #{}, V);
prepare_value_for_json(V) ->
    V.

%%--------------------------------------------------------------------
%% Internal: Restore state map from JSON.
%% Converts binaries back to atoms where appropriate.
%%--------------------------------------------------------------------
restore_from_json(Json) when is_map(Json) ->
    %% Keys that should be atoms
    AtomKeys = [class, area],
    maps:fold(fun(K, V, Acc) ->
        %% Convert key to atom if it's a known key
        Key = try binary_to_existing_atom(K, utf8) catch _:_ -> K end,
        Val = case lists:member(Key, AtomKeys) of
            true when is_binary(V) ->
                try binary_to_existing_atom(V, utf8) catch _:_ -> V end;
            _ ->
                restore_value_from_json(V)
        end,
        Acc#{Key => Val}
    end, #{}, Json).

restore_value_from_json(V) when is_list(V) ->
    [restore_value_from_json(X) || X <- V];
restore_value_from_json(V) when is_map(V) ->
    maps:map(fun(_K, Val) -> restore_value_from_json(Val) end, V);
restore_value_from_json(V) ->
    V.
