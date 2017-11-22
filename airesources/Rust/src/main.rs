/*
 * This is a Rust implementation of the Settler starter bot for Halite-II
 * For the most part, the code is organized like the Python version, so see that
 * code for more information.
 */

mod hlt;

use hlt::entity::{Entity, DockingStatus};
use hlt::game::Game;
use hlt::logging::Logger;

fn main() {
    let name = "Settler";

    // Initiailize the game
    let game = Game::new();

    // Initialize logging
    let mut logger = Logger::new(game.my_id);
    logger.log(&format!("Starting my {} bot!", name));

    // Retrieve the first game map
    let game_map = game.update_map();

    // You can preprocess things here,
    // you have 60 seconds...

    // Once you are done, send a "ready to work"
    game.send_ready(name);

    let mut command_queue = Vec::new();

    loop {

        // Update the game state
        let game_map = game.update_map();

        // Loop over all of our player's ships
        for ship in game_map.me().all_ships() {
            // Ignore ships that are docked or in the process of docking
            if ship.docking_status != DockingStatus::UNDOCKED {
                continue;
            }

            // Loop over all planets
            for planet in game_map.all_planets() {
                // Ignore unowned planets
                if planet.is_owned() {
                    continue;
                }

                // If we are close enough to dock, do it!
                if ship.can_dock(planet) {
                    command_queue.push(ship.dock(planet))
                } else {
                    // If not, navigate towards the planet
                    let navigate_command = ship.navigate(&ship.closest_point_to(planet, 3.0), &game_map, 90);
                    if let Some(command) = navigate_command {
                        command_queue.push(command)
                    }

                }
                break;
            }
        }
        // Send our commands to the game
        game.send_command_queue(&command_queue);
        command_queue.clear();
    }
}
