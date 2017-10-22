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
- game.send_command_queue(): Send commands to the game engine
- A map object, with the following variables: my id, width, height
- map.get_me(): Returns the player object
- map.get_player([player id]): Returns the player object designated by player id
- map.all_players(): Returns a list of player objects
- map.get_planet([planet id]): Returns a specific planet object
- map.all_planets(): Returns a list of all planet objects
- map.nearby_entities_by_distance([entity object]): returns all entities by distance ordinality
- map.obstacles_between([ship object], [target entity object], [object types to ignore]): Return a list of all entity objects between the ship and selected target
- A player object, with the following variables: id
- player.get_ship([ship id]): Returns the ship object aposite the ship id, which the player posesses
- player.all_ships(): Return a list of all (living) ships the player possesses
- entity object, with the following variables: x, y, radius, health, player, id
- entity.calculate_distance_between([target object]): Calculate the distance between the entity and the target object
- entity.calculate_angle_between([target object]): Calculate the angle between the entity and the target object
- entity.closest_point_to([target object], min_distance): Find the closest point between the entity and desired target, considering the min distance this point must be away form the radius of the target
- planet object, with the following variables: id, x, y, radius, num docking spots, current production, remaining production, health, owner
- planet.get_docked_ship([ship id]): Get the designated ship object with ship id which is docked on this planet
- planet.all_docked_ships(): Get a list of all ships docked on this planet
- planet.is_owned(): Return whether this ship is owned
- planet.is_full(): Return whether this planet may dock any further ships
- ship object, with the following variables: owner, id, x, y, radius, health, docking status, planet
- ship.thrust([magnitude], [angle]): Return a thrust command string for this ship considering the desired magnitude and angle
- ship.can_dock([planet object]): Returns whether the ship can dock at that planet
- ship.dock([planet object]): Return a docking command towards the desired planet object
- ship.undock(): Return an undocking command for this ship
- ship.navigate([target], [game map], [speed], [avoid obstacles], [max corrections], [angular step], [ignore ships], [ignore planets]): Return a naive navigate command for this turn, considering the desired target. Note that the algorithm for pathfinding here should not be optimized for any new language for the purpose of fairness

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
The map tokens are divided by specified "containers." Each container represents a item and apposite subitems ordered as follows:

#### Player
Note that items here are space delimited.
1. Number of players: "[(integer) number of players]"
2-5 (this may be between 1 and 4 items). Single player object containing the player's id, and the player's ships (this object is expanded on the next section): "[(integer) player id] [(ship) ships]"

#### Ship
The ships as extracted from player objects, and represented as follows:
1. Number of ships: "[(integer) number of ships]"
2-32767 (this may be between 0 and 32767 items). Single ship object containing the ship's id, x coordinate, y coordinate, health, x velocity (deprecated), y velocity (deprecated), docking status, docked planet, docking progress, and weapon cooldown: [(integer) ship id], [(floating point) x], [(floating point) y], [(integer) health], [(floating point) vel x=0], [(floating point) vel y=0], [(enum/integer) docked status], [(integer) docked_planet id], [(integer) docking progress], [(integer) cooldown] 

#### Planet
The planet object is extracted following the complete player objects:
1. Number of planets: "[(integer) number of planets]"
2-32767 (this may be between 1 and 32767 items). Singple planet object containing planet id, x coordinate, y coordinate, health, radius, number of docking spots, current production, remaining production (deprecated), health, whether the planet is owned, planet owner, number of docked ships, and the ids of those ships: [(integer) planet id], [(floating point) x], [(floating point) y], [(integer) health], [(floating point) radius], [(integer) docking], [(integer) current], [(integer) remaining], [(integer) is owned], [(integer) owner id], [(integer) number of docked ships] [(integer) id of ship 0] ... [(integer) id of ship x]

### Move command format
The move command, like all other commands, is comma separated and each command is distinct due to line delimiting. It is formatted as follows:

"t [(integer) ship id] [(integer) speed] [(integer) angle]"

Wherein the speed signifies the distance traversed in one turn and the angle is a number between 0 and 259 (inclusive) represented in degrees.

### Dock command format
The dock command, like all other commands, is comma separated and each command is distinct due to line delimiting. It is formatted as follows:

"d [(integer) ship id] [(integer) planet id]"

### Undock command format
The undock command is exactly equal to the dock command with a u replacing the d, and the fact that no planet id is necessary:

"u [(integer) ship id]"

### Submitting your new starter kit

Fork our [repo](https://github.com/HaliteChallenge/Halite-II/tree/master/airesources/Python3){:target="_blank"}, place your starter package in the `airesources/` folder, and send us a pull request! If we accept your PR, your starter package will be added to the site.

Note: please include the runGame.sh and runGame.bat scripts, you can look at the Python stater kit for inspiration.

[Discord]:
