cdef class Entity:
    """
    Then entity abstract base-class represents all game entities possible. As a base all entities possess
    a position, radius, health, an owner and an id. Note that ease of interoperability, Position inherits from
    Entity.

    :ivar id: The entity ID
    :ivar x: The entity x-coordinate.
    :ivar y: The entity y-coordinate.
    :ivar radius: The radius of the entity (may be 0)
    :ivar health: The planet's health.
    :ivar owner: The player ID of the owner, if any. If None, Entity is not owned.
    """
    #__metaclass__ = abc.ABCMeta

    cdef public double x, y, radius
    cdef public object health, owner, id

    cpdef double calculate_distance_between(Entity self, Entity target)

    cpdef double calculate_angle_between(Entity self, Entity target)

    cpdef Position closest_point_to(self, Entity target, int min_distance=*)

cdef class Position(Entity):
    pass
    # Should already be set up via extension


