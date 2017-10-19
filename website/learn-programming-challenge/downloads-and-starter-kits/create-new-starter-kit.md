---
layout: doc_page
title: Create a new starter kit
description: Learn how to create an AI starter kit in a new programming language for the Halite Challenge
image: assets/images/temp/bot_1.png
content: website
sort_key: 5
---
Thank you for your interest in creating a new Halite starter kit! We've put together everything you should need to know to create a Halite bot in a new programming language, but if you have any questions feel free to reach out via our [Discord Chat][Discord]{:target="_blank"} or email us at <halite@halite.io>.


## API Design

Please try to generally adhere to the API used in [Python3 starter kit](https://github.com/HaliteChallenge/Halite-II/tree/master/airesources/Python3){:target="_blank"}. This is as follows:

- game([Name of bot]): Initializes the game
- game.update_map(): Get latest map per turn
- game.all_ships(): All ships
- game.all_planets(): All planets
- game.[me].all_planets(): Get all planets for the player
- game.[me].all_ships(): Get all planets for the player
- planet entity: All properties of a planet
- ship entity: All properties of a ship
- ship.navigate(): Given a planet, ship or location navigate to that location avoiding collisions (best effort)
- game.send_command_queue(): Send commands to the game engine

Of course, if changes to this API will make the starter package fit more nicely into your language, feel free to make them. A Java API will not translate directly into Lisp nicely.

## Networking Overview
Bots communicate with the environment via stdin and stdout using series of space separated integers. There are two stages of communication - initialization and turn formats, and these are detailed below.

### Initialization
At the beginning of the game, bot is sent the following, with each item newline-terminated:

- A single integer representing their own tag within the game.
- Two integers representing the WIDTH and HEIGHT of the map.
- The initial game map.
- Every bot is expected to respond with a string representing their name (newline-terminated) within 30 seconds.

### Turn
Every turn, every bot is sent the the present game map (newline-terminated). Every bot is expected to respond with a set of moves (newline-terminated) within 2 seconds.

### Game map format

### Move command format

### Dock command format

### Submitting your new starter kit

Fork our [repo](https://github.com/HaliteChallenge/Halite-II/tree/master/airesources/Python3){:target="_blank"}, place your starter package in the `airesources/` folder, and send us a pull request! If we accept your PR, your starter package will be added to the site.

Note: please include the runGame.sh and runGame.bat scripts, you can look at the Python stater kit for inspiration.

[Discord]: