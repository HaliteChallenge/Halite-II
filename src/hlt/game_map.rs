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
    pub fn new(game: &'a Game, state: GameState) -> Self {
        Self { game, state }
    }

    pub fn all_planets(&self) -> &[Planet] {
        &self.state.planets
    }

    pub fn get_me(&self) -> &Player {
        let my_id = self.game.my_id;
        &self.state.players[my_id]
    }

    pub fn obstacles_between<T: Entity>(&self, ship: &Ship, target: &T) -> bool {
        for planet in self.all_planets() {
            if intersect_segment_circle(ship, target, planet, ship.radius() + 0.1) {
                return true;
            }
        }
        false
    }
}
