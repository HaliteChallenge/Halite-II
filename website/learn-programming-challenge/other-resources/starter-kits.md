---
layout: doc_page
title: Starter Kit Functionality
toc: false
---

Take advantage of the functionality your starter kits provide to make the most effective bots.

## Movement

All starter kits feature a basic, low-speed movement function that also performs basic collision avoidance. In Python, this is `hlt.move_to`, which takes a ship and the direction and acceleration you want to use. In C++ and Java, `adjust_for_collision`/`adjustForCollision` take an angle and acceleration and adjust the angle to avoid collisions.

These functions only issue a command for the current turn, and essentially function as a "move towards" command. This lets you recompute and update ship goals every turn without having to worry about long-term planning.

### Warp

The starter kits also have a basic movement function, called warp, that tries to take advantage of inertia to get you to places faster. This functionality takes control of your ship for you across multiple turns, accelerating and decelerating it to reach a target position.

In Python, simply call `hlt.warp`. At the beginning of each turn, make sure to call `hlt.update_warps` to get a list of commands that need to be issued to warping ships, and before issuing commands to a ship, call `hlt.is_warping` to make sure the ship isn't currently warping. (If you issue two commands to the same ship in a given turn, the game will eject your bot.)

In Java and C++, you will need to create an instance of the `BehaviorManager`, and use the `warpTo` or `warp_to` methods, respectively, to start warping. The `update` method issues the necessary commands each turn, while `isExecuting` or `is_behaving` let you check whether a ship is warping.

In all starter kits, warping falls back on `move_to` or `adjust_for_collision` at the end to reach the exact specified target point. Warps can be canceled, but the ship will brake before returning control to you.

Overall, warping is very naive–but using it well is critical to success. Improving it, or replacing it entirely, can give you a further edge.

## Utilities

All starter kits offer basic utilities including:

- Checking if a ship is in range to dock to a planet
- Computing the angle between two entities
- Generating an occupancy map to quickly check if grid squares are occupied
- Game parameter values to check things like weapon range, maximum acceleration, and resource consumption rates.