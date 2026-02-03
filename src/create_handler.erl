%%%-------------------------------------------------------------------
%%% @doc create_handler
%%% HTTP handler for the character creation page.
%%%
%%% Allows players to choose their character name and class:
%%% - Wizard: Fire staff, can find lightning and ice staves
%%% - Archer: Bow with fast projectiles
%%% - Warrior (Sword): 90-degree arc attack, high HP
%%% - Warrior (Dagger): Fast poke attack
%%% - Warrior (Spear): Medium range poke attack
%%% @end
%%%-------------------------------------------------------------------
-module(create_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handles GET and POST requests for character creation.
%% GET: Shows the character creation form
%% POST: Creates the character and redirects to game
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            Html = render_create_page(),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/html">>
            }, Html, Req0),
            {ok, Req, State};

        <<"POST">> ->
            %% Parse form data
            {ok, PostVals, Req1} = cowboy_req:read_urlencoded_body(Req0),
            Name = proplists:get_value(<<"name">>, PostVals, <<"Hero">>),
            ClassBin = proplists:get_value(<<"class">>, PostVals, <<"warrior_sword">>),

            %% Convert class to atom
            Class = case ClassBin of
                <<"wizard">> -> wizard;
                <<"archer">> -> archer;
                <<"warrior_sword">> -> warrior_sword;
                <<"warrior_dagger">> -> warrior_dagger;
                <<"warrior_spear">> -> warrior_spear;
                _ -> warrior_sword
            end,

            %% Create new player state
            GameState = game_state:new_player(Name, Class),
            GameStateWithInit = GameState#{initialized => true},

            %% Encode state to JSON for cookie
            JsonState = game_state:to_json(GameStateWithInit),
            EncodedState = uri_string:quote(JsonState),

            %% Set cookie using proper cowboy function
            Req2 = cowboy_req:set_resp_cookie(
                <<"game_state">>,
                EncodedState,
                Req1,
                #{path => <<"/">>, max_age => 31536000}
            ),

            %% Redirect to game
            Req = cowboy_req:reply(302, #{
                <<"location">> => <<"/">>
            }, <<>>, Req2),
            {ok, Req, State}
    end.

%%--------------------------------------------------------------------
%% @doc Renders the character creation HTML page.
%% @end
%%--------------------------------------------------------------------
render_create_page() ->
    <<"<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>Create Character - ERHTMX Adventure</title>
    <link rel=\"stylesheet\" href=\"/static/style.css\">
</head>
<body class=\"create-page\">
    <div id=\"create-container\">
        <h1>ERHTMX Adventure</h1>
        <h2>Create Your Character</h2>

        <form method=\"POST\" action=\"/create\" id=\"create-form\">
            <div class=\"form-group\">
                <label for=\"name\">Character Name</label>
                <input type=\"text\" id=\"name\" name=\"name\" maxlength=\"20\"
                       placeholder=\"Enter your name\" required>
            </div>

            <div class=\"form-group\">
                <label>Choose Your Class</label>
                <div class=\"class-grid\">
                    <div class=\"class-option\">
                        <input type=\"radio\" id=\"wizard\" name=\"class\" value=\"wizard\">
                        <label for=\"wizard\" class=\"class-card\">
                            <span class=\"class-icon\">&#128293;</span>
                            <span class=\"class-name\">Wizard</span>
                            <span class=\"class-desc\">Wields elemental staves with Fire, Lightning, and Ice spells. Lower HP but powerful magic.</span>
                            <span class=\"class-weapon\">Starting: Fire Staff</span>
                        </label>
                    </div>

                    <div class=\"class-option\">
                        <input type=\"radio\" id=\"archer\" name=\"class\" value=\"archer\">
                        <label for=\"archer\" class=\"class-card\">
                            <span class=\"class-icon\">&#127993;</span>
                            <span class=\"class-name\">Archer</span>
                            <span class=\"class-desc\">Fast projectiles with good range. Balanced HP and damage.</span>
                            <span class=\"class-weapon\">Starting: Bow</span>
                        </label>
                    </div>

                    <div class=\"class-option\">
                        <input type=\"radio\" id=\"warrior_sword\" name=\"class\" value=\"warrior_sword\" checked>
                        <label for=\"warrior_sword\" class=\"class-card\">
                            <span class=\"class-icon\">&#9876;</span>
                            <span class=\"class-name\">Swordsman</span>
                            <span class=\"class-desc\">Wide 90-degree arc attack. High HP and damage, medium speed.</span>
                            <span class=\"class-weapon\">Starting: Sword</span>
                        </label>
                    </div>

                    <div class=\"class-option\">
                        <input type=\"radio\" id=\"warrior_dagger\" name=\"class\" value=\"warrior_dagger\">
                        <label for=\"warrior_dagger\" class=\"class-card\">
                            <span class=\"class-icon\">&#128481;</span>
                            <span class=\"class-name\">Rogue</span>
                            <span class=\"class-desc\">Fast poke attacks with short range. High damage, high speed.</span>
                            <span class=\"class-weapon\">Starting: Dagger</span>
                        </label>
                    </div>

                    <div class=\"class-option\">
                        <input type=\"radio\" id=\"warrior_spear\" name=\"class\" value=\"warrior_spear\">
                        <label for=\"warrior_spear\" class=\"class-card\">
                            <span class=\"class-icon\">&#129717;</span>
                            <span class=\"class-name\">Lancer</span>
                            <span class=\"class-desc\">Medium range poke attacks. Balanced damage and speed.</span>
                            <span class=\"class-weapon\">Starting: Spear</span>
                        </label>
                    </div>
                </div>
            </div>

            <button type=\"submit\" class=\"start-btn\">Begin Adventure</button>
        </form>
    </div>
</body>
</html>">>.
