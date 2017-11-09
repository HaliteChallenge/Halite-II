###########################################################
## Generic Entity functions
###########################################################
abstract type Entity end

radius(entity::Entity) = entity.radius
id(entity::Entity) = entity.id

"""
    distance_between(e1, e2)

Calculates the eucledean distance between entities `e1` and `e2`.
"""
distance_between(e1::Entity, e2::Entity) = sqrt((e1.x - e2.x)^2 + (e1.y - e2.y)^2)

"""
    angle_between(e1, e2)

Calculates the angle between entities `e1` and `e2`.
"""
angle_between(e1::Entity, e2::Entity) = rad2deg(atan2(e2.y - e1.y, e2.x - e1.x)) % 360

"""
    closest_point_to(e1, e2, min_distance = 3.0)

Find the closest point to the given entity `e1` near the given target entity `e2`, outside its given radius, with an added fudge of min_distance.
"""
function closest_point_to(e1::Entity, e2::Entity, min_distance::Float64 = 3.0)
    angle = angle_between(e1, e2)
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
    docked::DockedStatus
    docked_planet_id::String
    progress::Int
    cooldown::Int
end
function Ship(owner_id::String, tokens::Vector{String})
    id = shift!(tokens)
    x = parse(Float64, shift!(tokens))
    y = parse(Float64, shift!(tokens))
    hp = parse(Int, shift!(tokens))
    _ = parse(Float64, shift!(tokens)) # deprecated vel_x
    _ = parse(Float64, shift!(tokens)) # deprecated vel_y 
    docked = DockedStatus(parse(Int, shift!(tokens)))
    docked_planet = shift!(tokens)
    progress = parse(Int, shift!(tokens))
    cooldown = parse(Int, shift!(tokens))

    Ship(owner_id, id, x, y, hp, Constants.SHIP_RADIUS, docked, 
        docked_planet, progress, cooldown)
end

"""
    can_dock(ship, planet)

Determine whether a ship is close enough to planet so it can dock.
"""
function can_dock(ship::Ship, planet::Planet)
    distance_between(ship, planet) <= radius(planet) + Constants.DOCK_RADIUS + Constants.SHIP_RADIUS
end


isdocked(ship::Ship) = ship.docked != UNDOCKED
dock(ship::Ship, planet::Planet) = "d $(ship.id) $(planet.id)"
undock(ship::Ship) = "u $(ship.id)"

thrust(ship::Ship, speed, angle) = "t $(ship.id) $(floor(Int, speed)) $(round(Int, angle))"

function nearest_unoccupied_planet(ship::Ship, planets)
    d = Inf
    p = nothing
    for planet in planets
        if isowned(planet)
            if planet.owner_id == ship.owner_id && isfull(planet)
                continue
            end
        end
        dist = distance_between(ship, planet)
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
