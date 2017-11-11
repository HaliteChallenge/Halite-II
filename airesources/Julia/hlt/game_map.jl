logger = get_logger("bot-logger")

struct GameMap
    id::String
    width::Int
    height::Int
    players::Dict{String, Player}
    planets::Dict{String, Planet}
end
function GameMap(id, width, height, map_string::String)
    game_map = GameMap(id, width, height, Dict{String, Player}(), Dict{String, Planet}())
    map_parse!(game_map, map_string)
    return game_map
end

"""
    map_parse!(game_map, map_string)

Parse the map description from the game. Changes definition of game_map.players and game_map.planets, so it should be used only on newly created GameMap. `map_string` contains the Halite engine outputs.
"""
function map_parse!(game_map::GameMap, map_string::String)
    tokens = Vector{String}(split(map_string))
    num_players = parse(Int, shift!(tokens))
    debug(logger, "Number of players: $num_players")
    for _ in 1:num_players
        player_id = shift!(tokens)
        player = Player(player_id)
        num_ships = parse(Int, shift!(tokens))
        for _ in 1:num_ships
            ship = Ship(player.id, tokens)
            player.ships[ship.id] = ship
            debug(logger, "Owner: $(ship.owner_id), Ship: $(ship.id), x: $(ship.x), y: $(ship.y)")
        end
        game_map.players[player_id] = player
    end
    num_planets = parse(Int, shift!(tokens))
    for _ in 1:num_planets
        planet = Planet(tokens)
        game_map.planets[planet.id] = planet
    end
    @assert isempty(tokens)
end

# Utility functions
"""
    get_player(game_map, player_id)
    get_player(game_map, entity)

Returns player defined by `player_id` or `entity.owner_id`.
"""
get_player(game_map::GameMap, player_id::String) = game_map.players[player_id]
get_player(game_map::GameMap, entity::Entity) = get_player(game_map, entity.owner_id)

"""
    get_me(game_map)

Returns the user's player.
"""
get_me(game_map::GameMap) = get_player(game_map, game_map.id)

"""
    all_players(game_map)

List of all players.
"""
all_players(game_map::GameMap) = values(game_map.players)

"""
    all_planets(game_map)

List of all planets.
"""
all_planets(game_map::GameMap) = values(game_map.planets)

"""
    get_planet(game_map, planet_id)
    get_planet(game_map, ship)

Returns planet defined by `planet_id` or `ship.docked_planet_id`.
"""
get_planet(game_map::GameMap, planet_id::String) = game_map.planets[planet_id]
get_planet(game_map::GameMap, ship::Ship) = get_planet(game_map, ship.docked_planet_id)

"""
    all_docked_ships(game_map, planet)

List of all docked ships, empty if planet is not owned.
"""
function all_docked_ships(game_map::GameMap, planet::Planet)
    planet.owned && return get_ship.(get_player(game_map, planet), planet.docked_ships_ids)
    return Vector{Ship}()
end

"""
    get_docked_ship(game_map, planet, ship_id)

Return docked ship. Throw error if ship is not docked on this planet.
"""
get_docked_ship(game_map::GameMap, planet::Planet, ship_id::String) = get_ship(get_player(game_map, planet), ship_id)

"""
    all_ships(game_map)

List of all ships in current game map.
"""
function all_ships(game_map::GameMap)
    ships = Vector{Ship}() 
    for player in all_players(game_map)
        append!(ships, all_ships(player))
    end
    return ships
end