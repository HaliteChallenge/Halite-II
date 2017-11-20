use hlt::parse::Decodable;
use hlt::entity::Entity;

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

impl Entity for Position {
    fn get_position(&self) -> Position {
        *self
    }

    fn get_radius(&self) -> f64 {
        0.0
    }
}
