---
layout: doc_page
title: Basic Game Rules
toc: false
description: Get an introduction to the rules of the game to win the Halite AI Programming Challenge.
sort_key: 0
---
Here is a quick overview of the rules of Halite II. If you want to get into the nitty-gritty, check out our [Game Rules Deep Dive][game-rules-deep-dive].

## Map at Game Start
Halite II is played on 2D map, which is set up as a continuos Cartesian plane (think of it has having x/y axes). The map is like a virtual game board, and does not wrap, meaning that game pieces can fall off the edges of the board, they don't appear on the opposite side like an old school arcade game.

<img src="" alt="Halite Game Board at Start of Game">

Maps are randomly and symmetrically generated, and consist of a number of planets at start. At the start, all planets are neutral and players own none of them. The sizes and locations of planets also varies map to map.

## Starting a game
Two or four players compete in a match. 
At the start of the game, each player will have 3 ships on the map.

<img src="" alt="Halite Game Board with 3 Ships per Player">

## Players each have a fleet of ships

As mentioned, players have 3 ships at the start of the game. You move ships around the map using three commands.
1. Ships can be commanded to move with a given angle and a velocity.
2. Ships can also “dock” to  a planet.
3. Ships can “undock” from a planet.

This is a ship:
<img src="" alt="Halite single ship">

The circle around the ship is its aura, which is uses to measure the offensive/defensive reach of the ship. Ships may be destroyed through battle or collision with each other and planets, and they have an initial health and lose health as they battle.

<img src="" alt="Halite ships in battle">

Ships collide when they try to occupy the same location on the map:

<img src="" alt="Halite ships explode">

## Planets are for producing new ships

<img src="/assets/images/tutorial-images/orange-planet.png" alt="Image of a halite planet">

Ships can 'dock' on planets to mine Halite and produce more ships.

<img src="/assets/images/tutorial-images/final-composite.png" alt="Halite ship docked on a planet">

Each player can dock ships simultaneously on multiple planets, but only ships from one player can dock on a particular planet at a time. The number of ships that can be docked to a planet is determined by the planet size.

When a ship docks on a planet, the planet produces ships for the docked player. Planets produce at a constant rate per ship with no limit to ship production.

<img src="" alt="Halite ships being produced by a planet">

Planets can be destroyed through ship collisions. Boom.
<img src="/assets/images/tutorial-images/planet-exploding.png" alt="Halite planet explosion>

## Win conditions
1. A player is the sole survivor.
2. A player occupies all planets.
3. If time runs out and neither of these conditions are met, tiebreaker rules apply:
    - First we check to see which surviving team has produced the most ships.
    - If still a tie, then we check to see which team has done the most battle damage (destroying ships with other ships through attacks or collision).
    - If still a tie, a random winner will be chosen, but this outcome has a very low probability of occurring given the other winning conditions.

[Deep Dive]: /learn-programming-challenge/game-rules/game-rules-deep-dive