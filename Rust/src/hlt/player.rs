use hlt::entity::Ship;
use hlt::parse::Decodable;

#[derive(PartialEq, Debug)]
pub struct Player {
    pub id: i32,
    pub ships: Box<[Ship]>,
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
        let ships = Box::parse(tokens);

        Self { id, ships }
    }
}
