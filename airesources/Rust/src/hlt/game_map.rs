
use hlt::game::Game;
use hlt::entity::{GameState, Planet};
use hlt::player::Player;
use hlt::collision::intersect_segment_circle;
use hlt::entity::{Entity, Ship};

pub struct GameMap<'a> {
    game: &'a Game,
    state: GameState,
}

impl<'a> GameMap<'a> {
    pub fn new(game: &Game, state: GameState) -> GameMap {
        return GameMap { game, state };
    }

    pub fn all_planets(&self) -> &Vec<Planet> {
        &self.state.planets
    }

    pub fn get_me(&self) -> &Player {
        let my_id = self.game.my_id;
        let player = &self.state.players[my_id];
        return player;
    }

    pub fn obstacles_between<T: Entity>(&self, ship: &Ship, target: &T) -> bool {
        for planet in self.all_planets() {
            if intersect_segment_circle(ship, target, planet, ship.get_radius() + 0.1) {
                return true;
            }
        }
        false
    }
}
