mod position;
mod docking_status;
mod ship;
mod planet;
mod game_state;

pub use self::position::Position;
pub use self::docking_status::DockingStatus;
pub use self::ship::Ship;
pub use self::planet::Planet;
pub use self::game_state::GameState;

/// As a base all entities possess a position, radius.
pub trait Entity: Sized {
    /// The object coordinates.
    fn position(&self) -> Position;

    /// The radius of the object (may be 0).
    fn radius(&self) -> f64;

    /// Calculates the distance between this object and the target.
    fn distance_with<T: Entity>(&self, target: &T) -> f64 {
        let Position(x1, y1) = self.position();
        let Position(x2, y2) = target.position();
        let (x, y) = (x2-x1, y2-y1);

        f64::sqrt(x*x + y*y)
    }

    /// Calculates the angle between this object and the target in degrees.
    fn angle_with<T: Entity>(&self, target: &T) -> f64 {
        let Position(x1, y1) = self.position();
        let Position(x2, y2) = target.position();

        (f64::atan2(y2-y1, x2-x1).to_degrees() + 360.0) % 360.0
    }

    /// Find the closest point to the given ship near the given target, outside its given radius,
    /// with an added fudge of min_distance.
    fn closest_point_to<T: Entity>(&self, target: &T, min_distance: f64) -> Position {
        let angle = target.angle_with(self);
        let radius = target.radius() + min_distance;
        let Position(target_x, target_y) = target.position();
        let x = target_x + radius * f64::cos(angle.to_radians());
        let y = target_y + radius * f64::sin(angle.to_radians());

        Position(x, y)
    }
}
