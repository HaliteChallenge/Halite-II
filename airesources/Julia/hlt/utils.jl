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
    b = -2.0 * (startl.x^2 - startl.x*endl.x - startl.x*circle.x + endl.x*circle.x + 
                startl.y^2 - startl.y*endl.y - startl.y*circle.y + endl.y*circle.y)
    c = (startl.x - circle.x)^2 + (startl.y - circle.y)^2

    if a < 1e-6
        # Start and end are the same point
        return distance_between(startl, circle) <= radius(circle) + fudge
    end

    # Time along segment when closest to the circle (vertex of the quadratic)
    t = min(-b / (2 * a), 1.0)
    if t < 0
        return false
    end

    closest_x = startl.x + dx * t
    closest_y = startl.y + dy * t
    closest_distance = distance_between(Position(closest_x, closest_y), circle)

    return closest_distance <= radius(circle) + fudge
end
