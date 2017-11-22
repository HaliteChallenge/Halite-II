use hlt::entity::Position;
use hlt::parse::Decodable;
use hlt::entity::Entity;

/// A planet on the game map.
#[derive(PartialEq, Debug)]
pub struct Planet {
    pub id: i32,
    pub position: Position,
    pub hp: i32,
    pub radius: f64,
    pub num_docking_spots: usize,
    pub current_production: i32,
    pub remaining_resources: i32,
    pub owner: Option<i32>,
    pub docked_ships: Box<[i32]>,
}

impl Planet {
    /// Determines if the planet has an owner.
    pub fn is_owned(&self) -> bool {
        self.owner.is_some()
    }

    /// Determines if the planet has been fully occupied (all possible ships are docked).
    pub fn is_full(&self) -> bool {
        self.docked_ships.len() >= self.num_docking_spots
    }
}

impl Decodable for Planet {
    fn parse<'a, I>(tokens: &mut I) -> Self
    where
        I: Iterator<Item = &'a str>,
    {

        let id = i32::parse(tokens);
        let position = Position::parse(tokens);
        let hp = i32::parse(tokens);
        let radius = f64::parse(tokens);
        let num_docking_spots = i32::parse(tokens) as usize;
        let current_production = i32::parse(tokens);
        let remaining_resources = i32::parse(tokens);
        let owner = Option::parse(tokens);
        let docked_ships = Box::parse(tokens);

        Self {
            id,
            position,
            hp,
            radius,
            num_docking_spots,
            current_production,
            remaining_resources,
            owner,
            docked_ships,
        }
    }
}

impl Entity for Planet {
    fn position(&self) -> Position {
        self.position
    }

    fn radius(&self) -> f64 {
        self.radius
    }
}
