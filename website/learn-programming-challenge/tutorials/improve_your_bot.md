---
layout: doc_page
title: Improve Your Bot
sort_key: 001
toc: true
---

In this tutorial, we’ll add a couple heuristics to the basic bot. This will hopefully help you understand Halite II better and set you on your way to dominating the universe.

## Prerequisites

Make sure you have read our [basic instructions]({{ site.baseurl }}/learn-programming-challenge/) and followed the steps to download your bot. Also, make sure you [understand the basic bot structure]({% link learn-programming-challenge/tutorials/understand_your_bot.md %})
Now open the `MyBot.py` file in your favorite editor and let’s get started!

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

### Fly Faster

When using `move_to`, instead of hardcoding a thrust, we can specify that the thrust is equal to the drag force, which is the fastest we can move without having to deal with inertia.

```python
command_queue.append(hlt.move_to(ship, angle, hlt.GameConstants.DRAG))
```

Remember, drag is applied at the end of the turn, so if you accelerate by less than or equal to the drag force, then at the end of the turn, drag will take over, and you won't have any momentum carry over into next turn.

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
    command_queue.append(hlt.move_to(ship, angle, 1))
```

