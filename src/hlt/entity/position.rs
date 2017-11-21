use hlt::parse::Decodable;
use hlt::entity::Entity;

/// A simple wrapper for a coordinate.
/// Intended to be passed to some functions in place of a ship or planet.
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Position(pub f64, pub f64);

impl Decodable for Position {
    fn parse<'a, I>(tokens: &mut I) -> Position
    where
        I: Iterator<Item = &'a str>,
    {
        let x = f64::parse(tokens);
        let y = f64::parse(tokens);

        Position(x, y)
    }
}

impl Entity for Position {
    fn position(&self) -> Position {
        *self
    }

    fn radius(&self) -> f64 {
        0.0
    }
}
