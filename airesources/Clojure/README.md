# Clojure Halite Starter Kit

## API

This starter kit keeps the game state in dynamic variables, as lispy languages
tends to do. All the variables are contained within `hlt.game-map`, and are
bound through the run loop in the main function.

The implementation is split into several packages:

- `hlt.networking` deals with reading and writing data to the game engine.
- `hlt.entity` contains all the different datatypes, constructors and utility
  functions on the datatypes.
- `hlt.math` has mathematical functions and datatypes, which can be used to find
  distances and positions betweeen and related to objects, and whether an object
  is on collision course on the way to another goal.
- `hlt.navigation` runs very simple navigation algorithms to detect where to
  move ships.
- `hlt.utils` contains a single logging function, which can be used for
  debugging

The functions should be fairly well documented, so you should be able to
experiment and play around with the functions without too much trouble.

## Running and Testing

Be sure to have `lein` and `halite` on your path. After that, you can run
`./run_game.sh` to see how well your bot performs against itself. You can replay
the game by submitting the `.hlt` file generated at the website
<https://halite.io/play-programming-challenge>.

## Deployment

When you feel your bot is ready to challenge other bots, you will have to zip
this directory and upload the zip file to
<https://halite.io/play-programming-challenge>.
