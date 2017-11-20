use hlt::entity::Ship;
use hlt::parse::Decodable;

#[derive(PartialEq, Debug)]
pub struct Player {
    pub id: i32,
    pub ships: Vec<Ship>,
}

impl Player {
    pub fn all_ships(&self) -> &[Ship] {
        &self.ships
    }
}

impl Decodable for Player {
    fn parse<'a, I>(tokens: &mut I) -> Self
    where
        I: Iterator<Item = &'a str>,
    {

        let id = i32::parse(tokens);
        let ships = Vec::parse(tokens);

        Self { id, ships }
    }
}
