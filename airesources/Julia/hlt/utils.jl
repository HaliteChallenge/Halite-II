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

function closest_point_to(e1::Entity, e2::Entity, min_distance::Float64 = 3.0)
    angle = angle_between(e1, e2)
    r = radius(e2) + min_distance
    x = e2.x - r * cos(deg2rad(angle))
    y = e2.y - r * sin(deg2rad(angle))
    return Position(x, y)
end

"""
    obstacles_between(game_map, entity1, entity2, ignore_ships, ignore_planets)
Check whether there is a straight-line path between two entities, without planetary or ships obstacles in between. Returns list of obstacles, empty if there is no obstacles were found.
"""
function obstacles_between(game_map, self, other, ignore_ships, ignore_planets)
    obstacles = Vector{Entity}()
    !ignore_ships   && check_collisions!(obstacles, all_ships(game_map), self, other)
    !ignore_planets && check_collisions!(obstacles, all_planets(game_map), self, other)
    return obstacles
end

function check_collisions!(obstacles, entitylist, self, other)
    for entity in entitylist
        any([self, other] .== entity) && continue
        if intersect_segment_circle(self, other, entity, radius(self) + 0.1)
            push!(obstacles, entity)
        end
    end
end

"""
intersect_segment_circle(start, end, circle, fudge=0.5)

Test whether a line segment and circle intersect.

# Arguments
- `start::Entity`: The start of the line segment. (Needs x, y attributes)
- `end::Entity`: The end of the line segment. (Needs x, y attributes)
- `circle::Entity`: The circle to test against. (Needs x, y, r attributes)
- `fudge::Entity`: A fudge factor; additional distance to leave between the segment and circle. (Probably set this to the ship radius, 0.5.)
"""
function intersect_segment_circle(startl::Entity, endl::Entity, circle::Entity, fudge=0.5)
    # Derived with SymPy
    # Parameterize the segment as start + t * (end - start),
    # and substitute into the equation of a circle
    # Solve for t
    dx = endl.x - startl.x
    dy = endl.y - startl.y

    a = dx^2 + dy^2
    # Start and end are the same point
    a < 1e-6 && return distance_between(startl, circle) <= radius(circle) + fudge

    b = -2.0 * (startl.x^2 - startl.x*endl.x - startl.x*circle.x + endl.x*circle.x + 
                startl.y^2 - startl.y*endl.y - startl.y*circle.y + endl.y*circle.y)

    # Time along segment when closest to the circle (vertex of the quadratic)
    t = min(-b / (2 * a), 1.0)
    t < 0 && return false

    closest_x = startl.x + dx * t
    closest_y = startl.y + dy * t
    closest_distance = distance_between(Position(closest_x, closest_y), circle)

    return closest_distance <= radius(circle) + fudge
end
