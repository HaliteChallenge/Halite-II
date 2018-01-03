from cpython cimport bool

from arithmetic cimport distance

def intersect_segment_circle(start, end, circle, *, fudge=0.5):
    return _intersect_segment_circle(start.x, start.y, end.x, end.y, circle.x, circle.y, circle.radius, fudge)


cdef bool _intersect_segment_circle(double startx, double starty, double endx, double endy, double circlex, double circley, double circlerad, double fudge=0.5):
    """
    Test whether a line segment and circle intersect.

    :param Entity start: The start of the line segment. (Needs x, y attributes)
    :param Entity end: The end of the line segment. (Needs x, y attributes)
    :param Entity circle: The circle to test against. (Needs x, y, r attributes)
    :param float fudge: A fudge factor; additional distance to leave between the segment and circle. (Probably set this to the ship radius, 0.5.)
    :return: True if intersects, False otherwise
    :rtype: bool
    """
    # Derived with SymPy
    # Parameterize the segment as start + t * (end - start),
    # and substitute into the equation of a circle
    # Solve for t
    cdef double dx, dy, a, b, c, t
    dx = endx - startx
    dy = endy - starty

    a = dx*dx + dy*dy
    b = -2 * (startx*startx - startx*endx - startx*circlex + endx*circlex +
              starty*starty - starty*endy - starty*circley + endy*circley)
    # Unused
    # c = (startx - circlex)**2 + (starty - circley)**2

    if a == 0.0:
        # Start and end are the same point
        return distance(startx,circlex,starty,circley) <= circlerad + fudge
        #return start.calculate_distance_between(circle) <= circlerad + fudge

    # Time along segment when closest to the circle (vertex of the quadratic)
    #t = min(-b / (2 * a), 1.0)
    t = -b/(2*a)
    if t < 0:
        return False
    elif t > 1.0:
        t = 1.0
    cdef double closest_x, closest_y, closest_distance
    closest_x = startx + dx * t
    closest_y = starty + dy * t
    closest_distance = distance(closest_x,circlex, closest_y,circley)

    return closest_distance <= circlerad + fudge
