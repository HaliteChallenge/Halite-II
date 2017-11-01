###########################################################
## Generic Entity functions
###########################################################
abstract type Entity end

radius(entity::Entity) = entity.radius
id(entity::Entity) = entity.id

"""
    calculate_distance_between(e1, e2)

Calculates the eucledean distance between entities `e1` and `e2`.
"""
calculate_distance_between(e1::Entity, e2::Entity) = sqrt((e1.x - e2.x)^2 + (e1.y - e2.y)^2)

"""
    calculate_angle_between(e1, e2)

Calculates the angle between entities `e1` and `e2`.
"""
calculate_angle_between(e1::Entity, e2::Entity) = rad2deg(atan2(e2.y - e1.y, e2.x - e1.x)) % 360

"""
    closest_point_to(e1, e2, min_distance = 3.0)

Find the closest point to the given entity `e1` near the given target entity `e2`, outside its given radius, with an added fudge of min_distance.
"""
function closest_point_to(e1::Entity, e2::Entity, min_distance::Float64 = 3.0)
    angle = calculate_angle_between(e1, e2)
    r = radius(e2) + min_distance
    x = e2.x + r * cos(deg2rad(angle))
    y = e2.y + r * sin(deg2rad(angle))

    return Position(x, y)
end

###########################################################
## Planet
###########################################################

struct Planet <: Entity
    id::String
    x::Float64
    y::Float64
    health::Int
    radius::Float64
    num_docking_spots::Int
    current_production::Int
    remaining_resources::Int
    owned::Bool
    owner_id::String
    docked_ships_ids::Vector{String}

    function Planet(tokens::Vector{String})
        id = shift!(tokens)
        x = parse(Float64, shift!(tokens))
        y = parse(Float64, shift!(tokens))
        hp = parse(Int, shift!(tokens))
        r = parse(Float64, shift!(tokens))
        docking = parse(Int, shift!(tokens))
        current = parse(Int, shift!(tokens))
        remaining = parse(Int, shift!(tokens))
        owned = shift!(tokens) != "0"
        owner_id = shift!(tokens)
        num_docked_ships = parse(Int, shift!(tokens))
        docked_ships_ids = Vector{String}(num_docked_ships)
        for i in 1:num_docked_ships
            docked_ships_ids[i] = shift!(tokens)
        end

        new(id, x, y, hp, r, docking, current, remaining, owned, owner_id, docked_ships_ids)
    end
end

"""
    isowned(planet)

Determines if the planet has an owner.
"""
isowned(planet::Planet) = planet.owned

"""
    isfull(planet)

Determines if the planet has been fully occupied (all possible ships are docked)
"""
isfull(planet::Planet) = length(planet.docked_ships_ids) >= planet.num_docking_spots

###########################################################
## Ship
###########################################################

struct Ship <: Entity
    owner_id::String
    id::String
    x::Float64
    y::Float64
    hp::Int
    radius::Float64
    vel_x::Float64
    vel_y::Float64
    docked::DockedStatus
    docked_planet_id::String
    progress::Int
    cooldown::Int
    
    function Ship(owner_id::String, tokens::Vector{String})
        id = shift!(tokens)
        x = parse(Float64, shift!(tokens))
        y = parse(Float64, shift!(tokens))
        hp = parse(Int, shift!(tokens))
        vel_x = parse(Float64, shift!(tokens))
        vel_y = parse(Float64, shift!(tokens))
        docked = DockedStatus(parse(Int, shift!(tokens)))
        docked_planet = shift!(tokens)
        progress = parse(Int, shift!(tokens))
        cooldown = parse(Int, shift!(tokens))

        new(owner_id, id, x, y, hp, Constants.SHIP_RADIUS, vel_x, vel_y, docked, docked_planet, progress, cooldown)
    end

end

"""
    isdocked(ship)

Test if current ship is undocked. Returns `true` if ship is `UNDOCKED`, `false` otherwise.
"""
isdocked(ship::Ship) = ship.docked != UNDOCKED

"""
    can_dock(ship, planet)

Determine whether a ship is close enough to planet so it can dock.
"""
function can_dock(ship::Ship, planet::Planet)
    calculate_distance_between(ship, planet) <= radius(planet) + Constants.DOCK_RADIUS
end

"""
    thrust(ship, speed, angle)
    
Generate a command to accelerate this ship.
"""
function thrust(ship::Ship, speed, angle)
    @sprintf("t %s %s %s", ship.id, floor(Int, speed), round(Int, angle))
end

"""
    dock(ship, planet)

Generate a command to dock to a planet.
"""
function dock(ship::Ship, planet::Planet)
    @sprintf("d %s %s", ship.id, planet.id)
end

"""
    undock(ship)

Generate a command to undock from the current planet.
"""
function undock(ship::Ship)
    @sprintf("u %s", ship.id)
end

function nearest_unoccupied_planet(ship::Ship, planets)
    d = Inf
    p = nothing
    for planet in planets
        if isowned(planet)
            if planet.owner_id == ship.owner_id && isfull(planet)
                continue
            end
        end
        dist = calculate_distance_between(ship, planet)
        if dist < d
            p = planet
            d = dist
        end
    end

    return p
end

###########################################################
## Position
###########################################################

struct Position <: Entity
    x::Float64
    y::Float64
end
radius(p::Position) = 0
id(p::Position) = "p"
