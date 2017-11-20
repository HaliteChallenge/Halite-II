use hlt::entity::Planet;
use hlt::player::Player;
use hlt::parse::Decodable;

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

        GameState { players, planets }
    }
}
