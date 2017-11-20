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

pub trait Entity: Sized {
    fn get_position(&self) -> Position;
    fn get_radius(&self) -> f64;

    fn calculate_distance_between<T: Entity>(&self, target: &T) -> f64 {
        let Position(x1, y1) = self.get_position();
        let Position(x2, y2) = target.get_position();
        f64::sqrt((x2-x1).powi(2) + (y2-y1).powi(2))
    }

    fn calculate_angle_between<T: Entity>(&self, target: &T) -> f64 {
        let Position(x1, y1) = self.get_position();
        let Position(x2, y2) = target.get_position();
        (f64::atan2(y2-y1, x2-x1).to_degrees() + 360.0) % 360.0
    }

    fn closest_point_to<T: Entity>(&self, target: &T, min_distance: f64) -> Position {
        let angle = target.calculate_angle_between(self);
        let radius = target.get_radius() + min_distance;
        let Position(target_x, target_y) = target.get_position();
        let x = target_x + radius * f64::cos(angle.to_radians());
        let y = target_y + radius * f64::sin(angle.to_radians());

        Position(x, y)
    }
}
