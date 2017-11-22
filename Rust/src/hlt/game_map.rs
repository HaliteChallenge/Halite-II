use hlt::game::Game;
use hlt::entity::{GameState, Planet};
use hlt::player::Player;
use hlt::collision::intersect_segment_circle;
use hlt::entity::{Entity, Ship};

/// Map which houses the current game information/metadata.
pub struct GameMap<'a> {
    game: &'a Game,
    state: GameState,
}

impl<'a> GameMap<'a> {
    pub fn new(game: &'a Game, state: GameState) -> Self {
        Self { game, state }
    }

    /// Return your own player.
    pub fn me(&self) -> &Player {
        let my_id = self.game.my_id;
        &self.state.players[my_id]
    }

    /// Returns all planets at the actual game state.
    pub fn all_planets(&self) -> &[Planet] {
        &self.state.planets
    }

    /// Returns all players at the actual game state including yourself.
    pub fn all_players(&self) -> &[Player] {
        &self.state.players
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
