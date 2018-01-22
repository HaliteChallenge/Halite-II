function send_command_queue(command_queue::Vector{String}) 
    print.(command_queue)
    print("\n")
end

dock(ship::Ship, planet::Planet) = "d $(ship.id) $(planet.id)"
undock(ship::Ship)               = "u $(ship.id)"
thrust(ship::Ship, speed, angle) = "t $(ship.id) $(floor(Int, speed)) $(round(Int, angle))"

# function thrust(group::Group, speed, angle) 
#     for ship in group.ships
#         thrust(ship::Ship, speed, angle)
#     end
# end

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
function navigate(game_map::GameMap, mobile, target;
            speed::Int = 0,
            avoid_obstacles = true,
            max_corrections::Int = 90,
            angular_step::Float64 = 1.0,
            ignore_ships = false,
            ignore_planets = false)
    max_corrections <= 0 && return ""

    dist = distance_between(mobile, target)
    angle = angle_between(mobile, target)
        if avoid_obstacles && !isempty(obstacles_between(game_map, mobile, target, ignore_ships, ignore_planets))
            new_target_dx = cos(deg2rad(angle + angular_step)) * dist
            new_target_dy = sin(deg2rad(angle + angular_step)) * dist
            new_target = Position(mobile.x + new_target_dx, mobile.y + new_target_dy)
            return navigate(game_map, mobile, new_target,
                            speed = speed,
                            avoid_obstacles = avoid_obstacles,
                            max_corrections = max_corrections - 1,
                            angular_step = angular_step,
                            ignore_ships = ignore_ships,
                            ignore_planets = ignore_planets)
        end
    speed = dist >= speed ? speed : floor(Int, dist)
    return thrust(mobile, speed, angle)
end
