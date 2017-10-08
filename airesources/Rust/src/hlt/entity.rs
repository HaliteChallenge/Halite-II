

use std::cmp::min;

use hlt::parse::Decodable;
use hlt::command::Command;
use hlt::constants::{DOCK_RADIUS, SHIP_RADIUS, MAX_SPEED};
use hlt::player::Player;
use hlt::game_map::GameMap;

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Position(pub f64, pub f64);

impl Decodable for Position {
    fn parse<'a, I>(tokens: &mut I) -> Position
    where
        I: Iterator<Item = &'a str>,
    {

        let x = f64::parse(tokens);
        let y = f64::parse(tokens);
        return Position(x, y);
    }
}

#[derive(PartialEq, Debug)]
pub enum DockingStatus {
    UNDOCKED = 0,
    DOCKING = 1,
    DOCKED = 2,
    UNDOCKING = 3,
}

impl Decodable for DockingStatus {
    fn parse<'a, I>(tokens: &mut I) -> DockingStatus
    where
        I: Iterator<Item = &'a str>,
    {

        let i = i32::parse(tokens);
        return match i {
            0 => DockingStatus::UNDOCKED,
            1 => DockingStatus::DOCKING,
            2 => DockingStatus::DOCKED,
            3 => DockingStatus::UNDOCKING,
            _ => panic!(format!("Not a valid docking status: {:?}", i)),
        };
    }
}

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
        self.calculate_distance_between(planet) <= (DOCK_RADIUS + planet.radius)
    }

    pub fn navigate<T: Entity>(&self, target: &T, game_map: &GameMap, max_corrections: i32) -> Option<Command> {
        if max_corrections <= 0 {
            return None
        }
        let angular_step = 1.0;
        let speed = MAX_SPEED;
        let distance = self.calculate_distance_between(target);
        let angle = self.calculate_angle_between(target);
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

        return Ship {
            id,
            position,
            hp,
            velocity_x,
            velocity_y,
            docking_status,
            docked_planet,
            progress,
            cooldown,
        };
    }
}

#[derive(PartialEq, Debug)]
pub struct Planet {
    pub id: i32,
    pub position: Position,
    pub hp: i32,
    pub radius: f64,
    pub num_docking_spots: i32,
    pub current_production: i32,
    pub remaining_resources: i32,
    pub owner: Option<i32>,
    pub docked_ships: Vec<i32>,
}

impl Planet {
    pub fn is_owned(&self) -> bool {
        self.owner.is_some()
    }
}

impl Decodable for Planet {
    fn parse<'a, I>(tokens: &mut I) -> Planet
    where
        I: Iterator<Item = &'a str>,
    {

        let id = i32::parse(tokens);
        let position = Position::parse(tokens);
        let hp = i32::parse(tokens);
        let radius = f64::parse(tokens);
        let num_docking_spots = i32::parse(tokens);
        let current_production = i32::parse(tokens);
        let remaining_resources = i32::parse(tokens);
        let owner = Option::parse(tokens);
        let docked_ships = Vec::parse(tokens);

        return Planet {
            id,
            position,
            hp,
            radius,
            num_docking_spots,
            current_production,
            remaining_resources,
            owner,
            docked_ships,
        };
    }
}

#[derive(PartialEq, Debug)]
pub struct GameState {
    pub players: Vec<Player>,
    pub planets: Vec<Planet>,
}

impl Decodable for GameState {
    fn parse<'a, I>(tokens: &mut I) -> GameState
    where
        I: Iterator<Item = &'a str>,
    {

        let players = Vec::parse(tokens);
        let planets = Vec::parse(tokens);

        return GameState { players, planets };
    }
}

pub trait Entity : Sized {
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

impl Entity for Ship {
    fn get_position(&self) -> Position {
        self.position
    }

    fn get_radius(&self) -> f64 {
        SHIP_RADIUS
    }
}

impl Entity for Planet {
    fn get_position(&self) -> Position {
        self.position
    }

    fn get_radius(&self) -> f64 {
        self.radius
    }
}

impl Entity for Position {
    fn get_position(&self) -> Position {
        *self
    }

    fn get_radius(&self) -> f64 {
        0.0
    }
}