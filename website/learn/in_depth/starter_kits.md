---
layout: doc_page
title: Starter Kit Functionality
toc: true
---

Take advantage of the functionality your starter kits provide to make the most effective bots.

## Movement

All starter kits feature a basic, low-speed movement function that also performs basic collision avoidance. In Python, this is `hlt.move_to`, which takes a ship and the direction and acceleration you want to use. In C++ and Java, `adjust_for_collision`/`adjustForCollision` take an angle and acceleration and adjust the angle to avoid collisions.

### Warp

## Utilities

All starter kits offer basic utilities including:

- Checking if a ship is in range to dock to a planet
- Computing the angle between two entities
- Generating an occupancy map to quickly check if grid squares are occupied

Additionally, the C++ and Java starter kits have various game constant values built-in, allowing your bot to check things like weapon range, maximum acceleration, and resource consumption rates.