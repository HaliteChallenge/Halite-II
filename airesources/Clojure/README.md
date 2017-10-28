# Clojure Halite Starter Kit

## API

This starter kit keeps the game state in dynamic variables, as lispy languages
tends to do. All the variables are contained within `hlt.game-map`, and are
bound through the run loop in the main function.

Most of the logic resides within `hlt.engine`, so when you want to tune your
bot, start looking there. The functions are well documented, so you should be
able to experiment and play around with the functions without being too
confused.

Other notable API functions in other namespaces include:
- `MyBot/with-updated-map` updates map into the dynamic variables, then performs
  the operations provided with the updated map.
- `hlt.networking/send-moves` sends a sequence of moves to the game engine. You
  can create moves through the functions `hlt.engine/dock-move`,
  `hlt.engine/undock-move` and `hlt.engine/thrust-move`, respectively.
- `hlt.utils/log` prints the arguments to the log file for this bot.

## Usage

Be sure to have `lein` and `halite` on your path. After that, you can run
`./run_game.sh` to see how well your bot performs against itself. You can replay
the game by submitting the `.hlt` file generated at the website
<https://halite.io/play-programming-challenge>.

When you feel your bot is ready to challenge other bots, you will have to zip
this directory and upload the zip file to
<https://halite.io/play-programming-challenge>.
