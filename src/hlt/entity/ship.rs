use std::cmp::min;
use hlt::constants::{DOCK_RADIUS, SHIP_RADIUS, MAX_SPEED};
use hlt::entity::{Position, Planet, DockingStatus};
use hlt::game_map::GameMap;
use hlt::command::Command;
use hlt::parse::Decodable;
use hlt::entity::Entity;

#[derive(PartialEq, Debug)]
pub struct Ship {
    pub id: i32,
    pub position: Position,
    pub hp: i32,
    pub velocity_x: f64,
    pub velocity_y: f64,
    pub docking_status: DockingStatus,
    pub docked_planet: Option<i32>,
    pub progress: i32,
    pub cooldown: i32,
}

impl Ship {
    pub fn thrust(&self, magnitude: i32, angle: i32) -> Command {
        Command::Thrust(self.id, magnitude, angle)
    }

    pub fn dock(&self, planet: &Planet) -> Command {
        Command::Dock(self.id, planet.id)
    }

    #[allow(dead_code)]
    pub fn undock(&self) -> Command {
        Command::Undock(self.id)
    }

    pub fn can_dock(&self, planet: &Planet) -> bool {
        self.distance_with(planet) <= (DOCK_RADIUS + planet.radius + SHIP_RADIUS)
    }

    pub fn navigate<T: Entity>(&self, target: &T, game_map: &GameMap, max_corrections: i32) -> Option<Command> {
        if max_corrections <= 0 {
            return None
        }
        let angular_step = 1.0;
        let speed = MAX_SPEED;
        let distance = self.distance_with(target);
        let angle = self.angle_with(target);
        if game_map.obstacles_between(self, target) {
            let new_target_dx = f64::cos((angle + angular_step).to_radians()) * distance;
            let new_target_dy = f64::sin((angle + angular_step).to_radians()) * distance;
            let Position(self_x, self_y) = self.position;
            let new_target = Position(self_x + new_target_dx, self_y + new_target_dy);
            self.navigate(&new_target, game_map, max_corrections - 1)
        } else {
            Some(self.thrust(min(speed, distance as i32), angle as i32))
        }
    }
}

impl Decodable for Ship {
    fn parse<'a, I>(tokens: &mut I) -> Ship
    where
        I: Iterator<Item = &'a str>,
    {
        let id = i32::parse(tokens);
        let position = Position::parse(tokens);
        let hp = i32::parse(tokens);
        let velocity_x = f64::parse(tokens);
        let velocity_y = f64::parse(tokens);
        let docking_status = DockingStatus::parse(tokens);
        let docked_planet_raw = i32::parse(tokens);
        let docked_planet = match docking_status {
            DockingStatus::UNDOCKED => None,
            _ => Some(docked_planet_raw),
        };
        let progress = i32::parse(tokens);
        let cooldown = i32::parse(tokens);

        Ship {
            id,
            position,
            hp,
            velocity_x,
            velocity_y,
            docking_status,
            docked_planet,
            progress,
            cooldown,
        }
    }
}

impl Entity for Ship {
    fn position(&self) -> Position {
        self.position
    }

    fn radius(&self) -> f64 {
        SHIP_RADIUS
    }
}
