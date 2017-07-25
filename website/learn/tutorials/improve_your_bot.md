---
layout: doc_page
title: Improve Your Bot
toc: true
---

In this tutorial, we’ll go through the code that powers our basic starter bots and add a couple heuristics to it. This will hopefully help you fully understand Halite II and set you on your way to dominating the universe.

## Prerequisites

Make sure you have read our [basic instructions]({{ site.baseurl }}/learn/) and followed the steps to download your bot.
Now open the `MyBot.py` file in your favorite editor and let’s get started!

## A look at the basic bot

Here’s our Python bot:

```python
import hlt


my_tag, map_size, initial_map = hlt.initialize("Settler")

while True:
    game_map = hlt.get_map()
    command_queue = []

    for ship in game_map.ships[my_tag].values():
        if ship.docked != "undocked":
            continue

        planets = game_map.planets.values()
        for planet in planets:
            if planet.owned:
                continue

            angle, distance = hlt.orient_towards(ship, planet)
            if hlt.can_dock(ship, planet):
                command_queue.append(hlt.dock(ship, planet))
            else:
                command_queue.append(hlt.move_to(ship, angle, 1))

            break

    hlt.send_command_queue(command_queue)
```

## Understanding the code

1. We start by importing the starter kit and initializing the bot, which tells the game what our bot is named. In return, the game environment assigns you your unique player ID and tells you what the map looks like.

    ```python
    import hlt
    my_tag, map_size, initial_map = hlt.initialize("Settler")
    ```

2. Then we start the main game loop. At the beginning of each turn, we read the new map from the game environment, and initialize a queue of commands to send back to the game.

    ```python
    while True:
        game_map = hlt.get_map()
        command_queue = []
    ```

3. Our starter bot then loops through each of the ships we own:

    ```python
       for ship in game_map.ships[my_tag].values():
    ```

4. The starter bot leaves docked ships still, since we can't issue any commands (besides undock) to them anyways:

    ```python
            if ship.docked != "undocked":
                continue
    ```

5. We then start to loop through all planets (regardless of ownership):

    ```python
            planets = game_map.planets.values()
            for planet in planets:
    ```

6. We ignore any planet that is owned (by us or anyone else):

    ```python
                if planet.owned:
                    continue
    ```

7. We figure out the angle and distance between the ship and the planet.

    ```python
                angle, distance = hlt.orient_towards(ship, planet)
    ```

8. We check to see if the ship is within range of the planet to dock. If so, we add the dock command to the queue of commands.

    ```python
                if hlt.can_dock(ship, planet):
                    command_queue.append(hlt.dock(ship, planet))
    ```

9. If we’re not in range to dock, we keep moving towards the planet until we can dock. Here, we use the `move_to` helper, which takes the angle and the thrust we want to use, and adjusts the angle to avoid collisions. Instead of hardcoding a thrust, we specify that the thrust is equal to the drag force, which is the fastest we can move without having to deal with inertia.

    ```python
                else:
                    command_queue.append(
                        hlt.move_to(ship, angle, hlt.GameConstants.DRAG))

                break
    ```


10. Finally we submit our queue of commands for our bot.
    ```python
        hlt.send_command_queue(command_queue)
    ```

## Improving the bot

### Go to the closest planets first

We can sort the planets by distance before iterating over them. The faster we can get to a planet and dock, the faster we'll get new ships to command.

```python
planets = game_map.planets.values()
sort_key = lambda planet: hlt.distance(ship, planet)
for planet in sorted(planets, key=sort_key):
```

### Skip a planet if no resources left

While we can win by trying to own every planet on the map, it's easier if we don't waste any ships docking to planets without resources unless we're specifically going for that strategy.

```python
if planet.owned or planet.remaining_production == 0:
    continue
```

### Undock ships that are docked to planets with no more resources

Similarly, if a ship is just hanging out at an empty planet, we might as well undock it and use it.

```python
if ship.docked == "docked" and game_map.planets[ship.planet].remaining_production == 0:
    command_queue.append(hlt.undock(ship))
    continue
```

### Use warp!

Warp lets us move far more quickly, allowing us to be more offensive and caputure territory more quickly. In Python, it also performs collision avoidance (this feature coming to other starter kits soon).

To use warp, there's a bit of setup needed. First off, it takes over control of the ship for you while the ship is warping---so before issuing commands to any ship, make sure it's not warping!

```python
if hlt.is_warping(ship):
    continue
```

Next, make sure to update the warps each turn. Before issuing your commands, just call the update function, which returns a list of commands to issue to warping ships. Extend your command list with this list so that the commands make it to your ships.

```python
command_queue.extend(hlt.update_warps())
hlt.send_command_queue(command_queue)
```

Finally, make sure to actually use warp! `warp` takes a target coordinate, not an angle and thrust. We'll use the `closest_point_to` helper function, which returns the coordinates that are 1) at a given distance from a target and 2) closest to the given source.

```python
# Find the coordinates which are DOCK_RADIUS units away
# from the planet surface and as close to possible to
# the ship.
target_x, target_y = hlt.closest_point_to(
    ship, planet, r=hlt.GameConstants.DOCK_RADIUS)
```

Then, if the ship is far away from the planet, we warp to the target coordinates instead.

```python
if hlt.can_dock(ship, planet):
    command_queue.append(hlt.dock(ship, planet))
elif distance > 10:
    hlt.warp(ship, target_x, target_y)
else:
    command_queue.append(hlt.move_to(ship, angle, 2))
```

