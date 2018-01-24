# Clojure Halite Starter Kit

## API

This starter kit is functional in the spirit of Clojure.

MyBot.clj contains bot initialization and the basic event loop, passing the new
game map each turn as an argument to the `next-moves` function, and emitting
those moves back to game.

The implementation is split into several namespaces:

- `hlt.my-bot` is the basic starter bot. It implements the `hlt.bot/Bot` protocol
   which is used during initialization and the event loop. The protocol's there
   to make it easier to develop multiple bots in while using the basic event loop.
- `hlt.networking` deals with reading and writing data to the game engine.
- `hlt.entity` contains all the different datatypes, constructors and utility
  functions on the datatypes.
- `hlt.math` has mathematical functions and datatypes, which can be used to find
  distances and positions betweeen and related to objects, and whether an object
  is on collision course on the way to another goal.
- `hlt.navigation` runs very simple navigation algorithms to detect where to
  move ships.
- `hlt.log` contains a single logging function which can be used for
  debugging. It takes a series of key-value pairs, e.g.,

        (hlt.log/log :msg "some message text" :game-map game-map)

  It emits these messages to a log file (by default in the `log` directory) using
  `prn-str` to make it easy to read log file contents for debugging.

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

Alternatively, from the command line:

    zip -r bot.zip project.clj target/* src/*

And using the [hlt client tool][hlt-client-tool] to upload your bot (remember to
authenticate first):

    hlt bot -b bot.zip

[hlt-client-tool]: https://halite.io/learn-programming-challenge/halite-cli-and-tools/halite-client-tools
