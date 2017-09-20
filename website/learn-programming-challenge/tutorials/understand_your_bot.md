---
layout: doc_page
title: Understand Your Bot
sort_key: 000
toc: true
permalink: improve
---

In this tutorial, we’ll go through the code that powers our basic starter bots. This will hopefully help you understand Halite II and get you started battling other bots.

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

9. If we’re not in range to dock, we keep moving towards the planet until we can dock. Here, we use the `move_to` helper, which takes the angle and the thrust we want to use, and adjusts the angle to avoid collisions. 

    ```python
                else:
                    command_queue.append(
                        hlt.move_to(ship, angle, 1))

                break
    ```


10. Finally we submit our queue of commands for our bot.
    ```python
        hlt.send_command_queue(command_queue)
    ```

