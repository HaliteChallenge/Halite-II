############################################################################################
# static entities
############################################################################################
abstract type Entity end
radius(entity::Entity) = entity.radius
id(entity::Entity) = entity.id
distance_between(e1::Entity, e2::Entity) = √((e1.x - e2.x)^2 + (e1.y - e2.y)^2)
angle_between(e1::Entity, e2::Entity) = rad2deg(atan2(e2.y - e1.y, e2.x - e1.x)) % 360

struct Planet <: Entity
    id::Int
    x::Float64
    y::Float64
    health::Int
    radius::Float64
    num_docking_spots::Int
    current_production::Int
    owned::Bool
    owner_id::Int
    docked_ships_ids::Vector{Int}
end
isowned(planet::Planet) = planet.owned
isfull(planet::Planet) = length(planet.docked_ships_ids) >= planet.num_docking_spots

struct Position <: Entity
    x::Float64
    y::Float64
end
radius(p::Position) = 0
id(p::Position) = -1

############################################################################################
# mobile entities
############################################################################################
abstract type MobileEntity <: Entity end

struct Ship <: MobileEntity
    owner_id::Int
    id::Int
    x::Float64
    y::Float64
    hp::Int
    docked::DockedStatus
    docked_planet_id::Int
    progress::Int
    cooldown::Int
end
radius(::Ship) = SHIP_RADIUS
isdocked(ship::Ship) = ship.docked != UNDOCKED
can_dock(s::Ship, p::Planet) = distance_between(s, p) <= radius(p) + DOCK_RADIUS + radius(s)

# struct Group <: MobileEntity
#     x::Float64
#     y::Float64
#     ships::Vector{Ship}
# end
# radius(::Group) = 3*SHIP_RADIUS+0.1

############################################################################################
# bookkeeping structs
############################################################################################
struct Player
    id::Int
    ships::Dict{Int, Ship}
end
Player(id::Int) = Player(id, Dict())
get_ship(player::Player, ship_id::Int) = player.ships[ship_id]
all_ships(player::Player) = values(player.ships)

struct GameMap
    id::Int
    width::Int
    height::Int
    players::Dict{Int, Player}
    planets::Dict{Int, Planet}
end
get_player(game_map::GameMap, player_id::Int) = game_map.players[player_id]
get_player(game_map::GameMap, entity::Entity) = get_player(game_map, entity.owner_id)
get_me(game_map::GameMap) = get_player(game_map, game_map.id)
all_players(game_map::GameMap) = values(game_map.players)
all_planets(game_map::GameMap) = values(game_map.planets)

get_planet(game_map::GameMap, planet_id::Int) = game_map.planets[planet_id]
get_planet(game_map::GameMap, ship::Ship) = get_planet(game_map, ship.docked_planet_id)
function all_docked_ships(game_map::GameMap, planet::Planet)
    planet.owned && return get_ship.(get_player(game_map, planet), planet.docked_ships_ids)
    return Vector{Ship}()
end
get_docked_ship(game_map::GameMap, planet::Planet, ship_id::Int) = get_ship(get_player(game_map, planet), ship_id)
function all_ships(game_map::GameMap)
    ships = Vector{Ship}() 
    for player in all_players(game_map)
        append!(ships, all_ships(player))
    end
    return ships
end

struct Game
    id::Int
    botname::String
    width::Int
    height::Int
    initial_game_map::GameMap
end
function Game(botname::String)
    map = parse_map!()
    Game(map.id, "$botname-$(map.id)", map.width, map.height, map)
end
