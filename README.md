# Halite

A project by Benjamin Spector and Michael Truell

![alt tag](https://raw.github.com/Sydriax/Halite/master/multimedia/Visualizer.png)

## Overview

Halite is a multi-player turn-based strategy game played on a rectangular grid. The objective of the game is to take over the entire map, in competition with every other player in the game. Players use their territory to gain strength, and their strength to gain territory; they must move intelligently on both the micro and macro scales to play effectively.

## Game Description

During a move, every piece you control can be given one of five moves: to move North, East, South, West, or to remain Still. When a piece remains where it is during a turn, two things will happen to it:

 - It will permanently increase its Strength by one.
 - It will receive, for the duration of that turn, a temporary strength boost.

Players gain pieces by simply moving their own pieces over Sites on the map. When a piece moves off of a Site, it leaves behind a piece with an identical Owner and with a Strength of 0, in doing so expanding the size of their territory.

When pieces from the same player try to occupy the same site, the resultant piece has the sum of their strengths. Real strengths are capped at 255 (although strength boosts can push strengths above that). When pieces from opposing players try to occupy either the same or adjacent sites, the battle will be resolved according to the relative strengths of the pieces, as each piece decreases the Strength of every adjacent or coinciding opposing piece by its own Strength. Pieces with a strength of 0 or less are removed from the game, excepting those which have not been in combat during that turn.

Players should note that the map does wrap around; if a piece at the top of the map moves North, it will reappear on the bottom of the map, and opposing pieces on the top and bottom of the map will engage in combat (provided that they are in the same column).

Throughout the game, scores are calculated as a sum of all territory that each player has controlled throughout the game. Players which survive until the end of the game receive a 2x bonus to their score. Scores are calculated purely for ranking players in games (i.e. First, Second, Third place...).

The game ends if one of two conditions are met:
 - Only one player is left.
 - A certain number of terms has been reached. This number will vary depending on the size of the map in question; a small map may end after a hundred moves or so, whereas a large map may take up to thousands.

## Folder Contents

- `airesources/` - The starter kits for writing bots
- `halite/` - The halite game engine and visualizer
- `multimedia/` - Photos and videos of a halite game
- `website/` - The website that will host the competition
- `worker/` - Code run on worker servers that will compile bots and run games safely
