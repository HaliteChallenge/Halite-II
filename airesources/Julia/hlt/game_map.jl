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
all_ships(game_map::GameMap) = [all_ships(player) for player in all_players(game_map)]

"""
    obstacles_between(game_map, entity1, entity2, ignore_ships, ignore_planets)

Check whether there is a straight-line path between two entities, without planetary or ships obstacles in between. Returns list of obstacles, empty if there is no obstacles were found.
"""
function obstacles_between(game_map::GameMap, e1::Entity, e2::Entity, ignore_ships::Bool, ignore_planets::Bool)
    obstacles = Vector{Entity}()
    if !ignore_ships
        for entity in all_ships(game_map)
            (e1 == entity || e2 == entity) && continue
            if intersect_segment_circle(e1, e2, entity, radius(e1) + 0.1)
                push!(obstacles, entity)
            end
        end
    end

    if !ignore_planets
        for entity in all_planets(game_map)
            (e1 == entity || e2 == entity) && continue
            if intersect_segment_circle(e1, e2, entity, radius(e1) + 0.1)
                push!(obstacles, entity)
            end
        end
    end
    return obstacles
end

"""
    navigate(game_map, ship, target)

Move a `ship` to a specific `target` position (Entity). It is recommended to place the position itself here, else navigate will crash into the target. If `avoid_obstacles` is set to true (default) will avoid obstacles on the way, with up to `max_corrections` corrections. Note that each correction accounts for `angular_step` degrees difference, meaning that the algorithm will naively try `max_correction` degrees before giving up (and returning empty string). The navigation will only consist of up to one command; call this method again in the next turn to continue navigating to the position.

# Arguments
- `game_map::GameMap`: The map of the game, from which obstacles will be extracted
- `ship::Ship`: ship to navigate
- `target::Entity`: The entity to which you will navigate
- `speed::Int`: The (max) speed to navigate. If the obstacle is nearer, will adjust accordingly.
- `avoid_obstacles::Bool`: Whether to avoid the obstacles in the way (simple pathfinding).
- `max_corrections::Int`: The maximum number of degrees to deviate per turn while trying to pathfind. If exceeded returns empty string.
- `angular_step::Float64`: The degree difference to deviate if the original destination has obstacles
- `ignore_ships::Bool`: Whether to ignore ships in calculations (this will make your movement faster, but more precarious)
- `ignore_planets::Bool`: Whether to ignore planets in calculations (useful if you want to crash onto planets)
"""
function navigate(game_map::GameMap, ship::Ship, target::Entity;
                 speed::Int = 0,
                 avoid_obstacles::Bool = true,
                 max_corrections::Int = 90,
                 angular_step::Float64 = 1.0,
                 ignore_ships::Bool = false,
                 ignore_planets::Bool = false)
    max_corrections <= 0 && return ""

    distance = distance_between(ship, target)
    angle = angle_between(ship, target)
    if avoid_obstacles && !isempty(obstacles_between(game_map, ship, target, ignore_ships, ignore_planets))
        new_target_dx = cos(deg2rad(angle + angular_step)) * distance
        new_target_dy = sin(deg2rad(angle + angular_step)) * distance
        new_target = Position(ship.x + new_target_dx, ship.y + new_target_dy)
        return navigate(game_map, ship, new_target,
                        speed = speed,
                        avoid_obstacles = avoid_obstacles,
                        max_corrections = max_corrections - 1,
                        angular_step = angular_step,
                        ignore_ships = ignore_ships,
                        ignore_planets = ignore_planets)
    end
    speed = distance >= speed ? speed : floor(Int, distance)
    return thrust(ship, speed, angle)
end
