use hlt::entity::{Entity, Position};

/// Test whether a line segment and circle intersect.
pub fn intersect_segment_circle<E: Entity, F: Entity, G: Entity>(start: &E, end: &F, circle: &G, fudge: f64) -> bool {
    let Position(start_x, start_y) = start.position();
    let Position(end_x, end_y) = end.position();
    let Position(circle_x, circle_y) = circle.position();
    let dx = end_x - start_x;
    let dy = end_y - start_y;

    let a = dx*dx + dy*dy;
    let b = -2.0 * (start_x*start_x - start_x*end_x - start_x*circle_x + end_x*circle_x +
              start_y*start_y - start_y*end_y - start_y*circle_y + end_y*circle_y);

    if a == 0.0 {
        // Start and end are the same point.
        return start.distance_with(circle) <= circle.radius() + fudge;
    }

    let &t = [-b / (2.0 * a), 1.0].iter().min_by(|x, y| x.partial_cmp(y).unwrap()).unwrap();
    if t < 0.0 {
        return false;
    }

    let closest_x = start_x + dx * t;
    let closest_y = start_y + dy * t;
    let closest_distance = Position(closest_x, closest_y).distance_with(circle);

    closest_distance <= circle.radius() + fudge
}
