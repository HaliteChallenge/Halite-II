---
layout: doc_page
title: The Halite Environment CLI
toc: true
---

The Halite environment is responsible for running games between bots and outputting appropriate data and files upon the end of a game. The downloadable version is the same version used on the servers.

It may be passed a number of flags, including:

- `-d`: allows the automatic input of the dimensions of the map. The following argument is expected to be a string containing the width and height (space-separated).
- `-t`: disables timeouts for the duration of the game.
- `-q`: turns on quiet output. Runtime game status information will not be printed, but at the end, a JSON summary of the game and various output files (replay and log file locations) will be printed.
- `-s`: provides the seed to the map generator. If this is not provided, it will use a time-based seed.
- `--print-constants`: print the game constants as JSON.
- `--constantsfile`: load the game constants values from a JSON file. This can be used to play with various scenarios. A particularly fun one is to reduce the resources needed to make a new ship.

## Examples

To run your bot against itself on a 40 by 40 map with no timeouts, run:

- Linux/macOS: `./halite -d '40 40' -t 'python3 MyBot.py' 'python3 MyBot.py'`
- Windows: `.\halite.exe -d '40 40' -t 'python3 MyBot.py' 'python3 MyBot.py'`

To run your Python bot against a Java bot (assuming it’s been compiled) on a 25 by 25 map with a predefined seed (2168), run:

- Linux/macOS: `./halite -d '25 25' -s 2168 'python3 PythonBot.py' 'java JavaBot'`
- Windows: `.\halite.exe -d '25 25' -s 2168 'python3 PythonBot.py' 'java JavaBot'`