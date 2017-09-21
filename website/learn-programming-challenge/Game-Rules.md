---
layout: doc_page
title: Game Rules
toc: false
description: Learn the rules of the game to win the Halite AI Programming Challenge.
---

## Map
The game is played on a non-wrapping, 2D Cartesian plane map.

## Matchup
Two or four players compete in a match. 

## Players each have a fleet of ships
1. Players have 3 ships at game start
2. Players move ships around a board using three commands
    - Ships can be commanded to move with a given angle and a velocity
    - Ships can also “dock” to  a planet
    - Ships can “undock” from a planet
3. Ships may be destroyed through battle or collision with each other and planets
4. Ships have an initial health and lose health as they battle

## Planets are for producing new ships
1. At the start of a game, all planets are neutral and players own none of them. The sizes and locations of planets also varies map to map
2. Only one team can dock on a planet at a time. 
3. Number of ships that can be docked to a planer is determined by the planets size.
4. When a ship docks on a planet, the planet produces ships for the docked player
5. Planets produce at a constant rate per ship with no limit to ship production
6. Planets can be destroyed through collisions with ships

## Win conditions
1. They are the sole survivor
2. They occupy all planets
3. If time runs out and neither of these conditions are met, tiebreaker rules apply:
    - First we check to see which surviving team has produced the most ships
    - If still a tie, then we check to see which team has done the most battle damage (destroying ships with other ships through attacks or collision)
    - If still a tie a random winner will be chosen, but this outcome has a very low probability of occurring given the other winning conditions.