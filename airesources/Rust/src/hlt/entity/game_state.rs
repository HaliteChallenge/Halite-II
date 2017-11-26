use hlt::entity::Planet;
use hlt::player::Player;
use hlt::parse::Decodable;

#[derive(PartialEq, Debug)]
pub struct GameState {
    pub players: Box<[Player]>,
    pub planets: Box<[Planet]>,
}

impl Decodable for GameState {
    fn parse<'a, I>(tokens: &mut I) -> Self
    where
        I: Iterator<Item = &'a str>,
    {
        let players = Box::parse(tokens);
        let planets = Box::parse(tokens);

        Self { players, planets }
    }
}
