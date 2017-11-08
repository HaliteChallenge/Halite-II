module Halite


@enum DockedStatus UNDOCKED=0 DOCKING=1 DOCKED=2 UNDOCKING=3

struct CONST
    #: Max number of units of distance a ship can travel in a turn
    MAX_SPEED::Int
    #: Radius of a ship
    SHIP_RADIUS::Float64
    #: Starting health of ship, also its max
    MAX_SHIP_HEALTH::Int
    #: Starting health of ship, also its max
    BASE_SHIP_HEALTH::Int
    #: Weapon cooldown period
    WEAPON_COOLDOWN::Int
    #: Weapon damage radius
    WEAPON_RADIUS::Float64
    #: Weapon damage
    WEAPON_DAMAGE::Int
    #: Radius in which explosions affect other entities
    EXPLOSION_RADIUS::Float64
    #: Distance from the edge of the planet at which ships can try to dock
    DOCK_RADIUS::Float64
    #: Number of turns it takes to dock a ship
    DOCK_TURNS::Int
    #: Number of turns it takes to create a ship per docked ship
    BASE_PRODUCTIVITY::Int
    #: Distance from the planets edge at which new ships are created
    SPAWN_RADIUS::Float64
end
Constants = CONST(7, 0.5, 255, 255, 1, 5.0, 64, 10.0, 4.0, 4, 6, 2.0)

include("entity.jl")
include("player.jl")
include("game_map.jl")
include("game.jl")

export Game, Constants, DockedStatus
export start_game, thrust, dock, undock
export update_map, all_players, all_planets, all_ships, all_docked_ships, get_player, get_me, get_planet, get_docked_ship
export isdocked, isowned, isfull, can_dock
export navigate, send_command_queue, closest_point_to, nearest_unoccupied_planet

end
