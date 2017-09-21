---
layout: doc_page
title: Game Rules Deep Dive
toc: false
description: Learn the details of the rules of the game to win the Halite AI Programming Challenge.
---

## Start of Game
All games are played with either 2 or 4 players. At the beginning of the game, for their first turn, all bots receive the initial game state, as well as their player ID, and have 30 seconds to prepare and send back their bot name.

## Map
Maps are rectangular grids with 3:2 aspect ratio (e.g. 288 x 192 units), using a 2D Cartesian continuous non-wrapping grid. The largest board is 384 x 256 units, the smallest is 240 x 160 units. The coordinates are treated as a real Cartesian plane, and game entities can have floating-point positions. All distances are Euclidean (sqrt(dx^2 + dy^2)), and are between the centers of entities. If a ship goes off the grid it is considered dead, since the grid does not wrap.

Maps are generated with a random number of planets, distributed symmetrically across the board varying by game. Planets are perfectly circular, with a radius ranging from 3 to 8 units, dependent on the map size (planets on a given map range in size, but the average size is determined by map size). The map generation algorithm aims to place more planets for 4-person games than 2-person games.
 
## Teams at Start of Game
Each player begins with three ships, arranged along the vertical axis with one unit of space between the centers of the ships, and no planets. In the four-player case, each player begins in the center of a quadrant of the map; in the two-player case, each player begins in the center of one of the halves of the map, mirrored either vertically or horizontally.

Each initial ship starts with the maximum 255 health points and 0 velocity.

## Turns
Halite II is a simultaneous turn-based game. The game proceeds until a bot wins, or until the turn limit is reached. The turn limit is calculated as 100 + sqrt(min(mapWidth * mapHeight)).

At the beginning of each turn, all bots receive the current game state, and have a limited time (1500 milliseconds) to issue one command (thrust, dock, or undock). Any bot that issues a malformed command or does not respond within the allotted time is ejected.

Turns are calculated using the following order of steps:
1.    The status of any docked ships is updated.
2.    Player commands are processed. For instance, a new thrust command will instantaneously update the ship velocity.
3.    Movement, collisions, and attacks are resolved simultaneously.
4.    Damage from combat is applied.
5.    Planet resources/ship creation is updated.
6.    Weapon cooldowns are updated.

These steps may be referred to as ‘substeps’. There is no minimum time for a step, but times are rounded to four decimal places.
 
This calculation is most relevant to battles and other events that happen simultaneously. See relevant sections below for more details how these calculations impact [battles], [docking], and [collision].

## Entities & Game Mechanics
The entities in Halite II are ships and planets. The key mechanics are movement, docking/mining, collision and combat.

### Ships
Each ship occupies a perfectly circular area with radius 0.5 units. Ships have:
* Health, ranging from 0 to 255 integral points, and starting at 255 points.
* Velocity, ranging from 0 to 15 integral units/turn in any direction. (The velocity is stored as two floating-point components, representing the x and y directions; the magnitude of the resulting velocity vector is scaled as to not exceed 15.)
* A weapon, with a reach of 5 units in all directions, dealing 64 damage/turn.

### Movement
A thrust command will set a direction and a velocity for a ship by specifying an angle in degrees (integers) and a velocity (integer ranging 0 to 15). Bots must be given a thrust command every turn to continue to move (stateless).

Beyond basic thrust, the API provides some additional helper methods for pathfinding.
1. obstacles_between: determines whether there are obstacles between two designated entities (ships/planets/positions).
2. nearby_entities_by_distance: a list of entities on the map with their distance to source object
3. calculate_distance_between: given two entities, calculate distance between them
4. calculate_angle_between: given two entities, return the angle (in degrees) between them
5. closest_point_to: returns a point near a given entity closest to the source entity
6. navigate: given a position, will navigate a user towards a given location using other pathfinding methods. Navigate is a stateless method, you must continue to call it each turn until you arrive at your destination, and can always choose not to call it, being able to easily change directions between turns. If avoid_obstacles is set to True (default) will avoid obstacles on the way, with up to max_corrections corrections

See [API documentation] for full details and tutorials for examples in practice. In order be successful at Halite II, players will need sophisticated pathfinding algorithms, but new players are encouraged to try out our navigate helper function as an easier starting point.

### Planets
Each planet occupies a perfectly circular area. Planets have:
* Radius, which determines their size
* Health, with the maximum value depending on their radius
* Unlimited resources used to create new ships
* An owner (start the game without an owner)
* A list of docked ships
* No velocity, no weapons

### Docking/Mining
To take control of a planet, a player must dock a ship to the planet. Only one team can dock on a planet at a time.

**Docking:** Once a ship moves within 4 units of a planet (ie. 4 + radius distance from center of planet), it can be commanded to dock, which will cause the ship to begin the docking process. The planet must be unoccupied or owned by player currently intending to dock. The ship must also be stationary. If the ship is too far away or not stationary or the planet is occupied, the dock command does nothing.

If two ships from different teams both issue docking commands on an unoccupied planet during the same turn, they will battle. If they both continue to issue docking commands, they will continue to battle. During these turns, the ships are not yet docking and therefore maintain their defenses (described under docking section below) until they start to successfully dock.

Once a planet has been docked by one ship, the owning player may continue to dock ships to the planet until the limit of ships per that planet has been reached. The maximum number of ships that can be docked to a planet is equal to the radius. E.g. a planet with a radius of 3 units may have up to 3 ships docked to it at once.

If a ship attempts to dock on a full side of a planet (all spots full) the docking process will move them to the closest unoccupied spot on the planet until the planet is full. If the planet is full, the dock command does nothing.

It takes 5 complete turns for the ship to dock. After these 5 turns the ship is considered fully docked. During this time, a docking ship may be attacked and the ship has no defenses. (i.e. if attacked, the ship is dealt damage but does not fire) A docking ship cannot be commanded.  A docked ship and an undocking ship also have no defenses (see more below).
 
**Mining:** Once docked, a ship automatically starts mining a planet, and the planet starts producing new ships. The rate of production is dependent on the number of ships docked to a planet: each docked ship contributes 6 units per turn to ship production and it takes 72 units to produce a ship. E.g. a planet with 12 ships docked would make a ship every turn, with 6 ships docked every two turns.
Newly created ships will appear within 2 units from the surface of the planet (i.e. within 2 + radius units of the center of the planet), in the unoccupied grid square closest to the center of the map. The ship will start with full health and zero velocity.

**Undocking:** A fully docked ship may be undocked from the planet, by issuing another command to ‘undock’. The ship will begin the undocking process, which takes 5 complete turns, after which the player will have full control over the ship again. An undocking ship will remain defenseless as it was while docked or docking. A ship will not move while undocking, so once undocked the ship will be in the same location it was in while docked.

### Ship-Planet Collision
Ships can do combat with planets by crashing into them (occupying locations on the perimeter of planets).

Like ships, planets also have health points. Planets start with a number of health points proportional to their radius: 255 points per unit of radius (thus, a radius 3 planet has 765 health). A ship can damage or destroy a planet by colliding with it; the planet takes 1 point of damage for each point of health the ship had. Since ships have up to 255 health, this means a planet can absorb one collision with a full-health ship per unit of radius it has. For instance, a radius 3 planet will explode if 3 full-health ships collide with it.

When a planet dies, it explodes, dealing damage to any ships or planets within 5 units of the planet surface. The damage scales linearly with distance from the surface, beginning at 255 damage when adjacent to the planet and ending at 51 damage if 5 units away.

### Ship-Ship Combat & Collision
Ships automatically fight each other when they come into close distances. (5 units from the center of the ship, represented on the board by the aura around the ship) When ships come into contact, they do 64 units of damage per turn to each other.

Ships that try to occupy the same spot on the board will both explode. I.e. if the ships are moving at a high velocity towards each other, ships will start to fight but will collide before getting to zero strength. Ship collisions do no damage to any ships or planets other than the two ships themselves. (A very rare edge case is when two planets collide with each other at the same time that they collide with a planet. In this case, they would collide with the planet and destroy each other at the same time).

If multiple enemy ships are within range simultaneously for a turn, the damage is evenly spread between all ships. (During processing, each ship accumulates damage in floating-point precision, which is then rounded down and applied at the end of each substep).  

For example, if two ships from bot A are within the range of one ship from bot B for a given turn, each ship with full strength, then each of the bot A ships will receive 32 units of damage, while bot B’s ship will receive 128 units of damage. Bot B’s attack will be split between the bot A ships, while bot B’s ship takes the full brunt of both of the attacks from bot A.

```
...A.A....
....B.....
..........
```

Once a ship fires its weapon, it may not attack again for the rest of the turn. Because ships attack immediately as soon as a ship comes into range, the direction of approach is crucial. 

Consider ships A, B, and C arranged like so, where A is owned by player 1 and B and C are owned by player 2:
```
..........
A......B.C
..........
```

Within one turn, if A moves five units to the right, on the substep where it has moved 2 units to the right, reaching firing distance of the B ship, those two ships will attack each other, dealing full damage (64 each).
```
..........
..a....b.C
..........
```
Within this turn, A will continue moving, and as it approaches ship C, then ship C will fire, dealing an additional 64 damage. A will not fire again because it can only fire once per turn. On the next turn, if A is still within range of B and C, A will split its damage between B and C as described above.
 
However, consider this arrangement:
```
.......B..
A.........
.......C..
```

If A moves as before, it will reach a point where it enters range of B and C simultaneously. Then both B and C will take 32 damage (half of A’s attack), but A will take 128 damage (the sum of B and C’s attacks).




