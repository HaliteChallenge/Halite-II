Halite II OCaml starter package
-------------------------------

This is a starter package for Halite II on halite.io written in OCaml.

Note the `initial_bot_setup` function - you have one minute to spend processing the initial game state, this is where you would place that setup logic.

This bot mostly matches the Python starter package. Differences include:
 - Functions for interacting with the game are found in game.ml or networking.ml instead of separating them by Class (ship, planet, player, etc...).
 - The `obstacles_between` function expects a list of potential obstacle entities, instead of a expecting a list of types to exclude. The calling function should do any filtering required (the `Game.ship_entities` and `Game.planet_entities` functions, as well as `Game.all_ships`, `state.player.(n).ships`, and `Game.all_planets` can provide the source data for filtering).
 - A negative number for `entity.owner` indicates that it is not owned.

The data structures all use mutable variables.

