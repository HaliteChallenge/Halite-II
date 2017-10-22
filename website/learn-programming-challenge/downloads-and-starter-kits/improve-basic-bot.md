---
layout: doc_page
title: Improve your Basic Bot
description: Tutorial to learn how to improve a basic Halite AI bot with a few heuristics as an easy way to get started playing in the Halite AI competition.
image: assets/images/temp/bot_1.png
content: website
sort_key: 6
---

In this tutorial, we’ll add a couple heuristics to the basic bot. This will hopefully help you understand Halite II better and set you on your way to dominating the universe.

## Prerequisites for this Tutorial

Make sure you have read our [basic instructions]({{ site.baseurl }}/learn-programming-challenge/) and followed the steps to download your bot. Now open the `MyBot.py` file in your favorite editor and let’s get started!

## Tips for beginners

For those just getting started, we've provided some code examples you could use to improve the basic bots.

### Up Close and Personal

We can sort the planets by distance before iterating over them. The faster we can get to a planet and dock, the faster we'll get new ships to command. You can get the distance from each ship to each map entity, sort by distance, then find which of the closest entities is a planet.

``` python
entities_by_distance = game_map.nearby_entities_by_distance(ship)
nearest_planet = None
for distance in sorted(entities_by_distance):
  nearest_planet = next((nearest_entity for nearest_entity in entities_by_distance[distance] if isinstance(nearest_entity, entity.Planet)), None)
  if nearest_planet:
    break
```

### Divide and Conquer

Rather than having all your ships travel to the same single planet, you could diversify and send ships to different planets. Without diversifying, if the single planet you're attempting to conquer has already been conquered by an enemy in the midst of your voyage, you will have wasted many turns. But with this strategy, you have a better chance of landing on a vacant planet. You can get all the planets, get all the ships, and then assign them to each other in the map's default order on a round robin basis.

``` python
planets = game_map.all_planets()
ships = game_map.get_me().all_ships()
for current in range(0, len(ships)):
  ships[current].navigate_to(ship.closest_point_to(planets[current%len(planets)]), game_map, speed=hlt.constants.MAX_SPEED/2))
```

### Be Large and In Charge

Now that you have sent each of your ships to one of the closest planets, you must define a way to determine which planets are most gainful when you have more than one choice. The largest planets grant you the greatest tactical advantage, due to their innate ability to produce the most. Going towards those planets is a big step towards winning games.

``` python
largest_planet = max(planet.radius for planet in game_map.all_planets())
```

## Advanced strategies

For those of you who are looking for ideas to take your bot even further, we've provides some suggestions below. But we're not going to provide any code for you - you'll have to do that part yourself!

### Locked and Loaded

When ships are docked/docking/undocking, they can't move or defend themselves. Morever, docking and undocking events cannot be cancelled, so upwards of 10 turns of defenselessness arise from a docking procedure; considering that it takes one ship 4 turns to destroy another ship, this should provide ample time. A good approach would be to wait until the ship starts to dock to attack. Through this means, you can destroy a ship without taking any damage yourself.

### Band Together

Due to the way in which ships fight, having the greater number helps in more way than one: consider a situation with two of player A's ships and two of player B's ships. If player A's ships are close together but player B's ships are far apart, player A's ships will first fight with one of player B's ships, and will then fight with the second of player B's ships. For each engagement, player A's ships will deal 128 damage per turn (64 per ship) while receiving 32 each (1/2 of the 64). It will takes two turns to destroy each of player B's ships at this rate, and after four turns, player A will have dealt 512 damage while receiving 256. The damage decreases the greater the advantage. Having 4 ships together is enough to actually prevent a collision from an enemy's ship, because 4 ships can destroy an enemy ship in one turn. The ship would be destroyed before it could collide.

As such, it can be seen as a great advantage to band together. Many formations for such are viable, but beware of ship swarm collisions, which are very likely if coordination isn't taken into account.

### A Journey of a Thousand Li Starts Beneath One's Feet

As it stands, the game's pathfinding (i.e. the navigate function) is quite naive (and purposely so) in its calculations - it simply tries all possible lines in angular order until it finds a suitable unblocked path. Its process is to continuously tests collision against all other objects. It does not account for past or future turns, only the turn in which it was called, and may not be able to find a useful path in a reasonable number of turns. You will find this inefficient both in terms of achieved outcome and processing speed.

Even given its naivety, it does a significant number of calculations. This will also be detrimental as you advance in the ranks, as the more ships you must test collisions against, the more likely you will time out. Your best path to victory is to improve your pathfinding. Many possible solutions exist, all with their pros and cons. The following are a number of viable approaches to improve the algorithm:

*1. Create a loose hit-map:* Having a near-accurate means of determining collisions without needing complex calculations is very useful, as your tests turn more complex. Assuming a discrete MxN array, a boolean hitmap can be devised with minimal loss of resolution.
*2. Graph-Theorize:* Now that you have a hit map, you can technically transform every path into a graph at the discrete points. Through those points you can now trace paths such that they don't interfere with each other (i.e. fewer ship collisions between paths).
*3. Be A-Star:* With a graph and a hit map, you can now use your favorite pathfinding algorithms to iteratively improve your movements.

### Molecular Detachment Device

Planets are often useful for the peaceful task of mining Halite; this, however isn't the only use you can get from a planet: planets, upon their destruction, possess an explosion radius. Any ship caught in the path of this radius is automatically destroyed, including docked ships. As such, it is sometimes advantageous to target planets rather than ships: a densely populated planet may easily turn the tides of a game if destroyed. However, do be careful, as your ships may also be caught in the explosion. 
