---
layout: doc_page
title: Using the API
sort_key: 002
toc: true
---

In this tutorial, we’ll go over the basic universal API for Halite. All starter kits should contain equal capabilities which enable optimal interfacing with the Halite Game, so whether you're a Python Guru or a C++ beginner, this document should help you equally. Examples in this document will reference the python nomenclature however, but all names should be very similar if not equal across all languages.

## Prerequisites

Make sure you have read our [basic instructions]({{ site.baseurl }}/learn/) and followed the steps to download your desire starter kit.

## The Library

To start you need to include the halite library into your code. If you're using Python, simply do:

```python
import hlt
```

## The Game

The entire Halite game is contained within the Game object. When the game starts you instantiate the Game object with your bot name as a string:

```python
game = hlt.Game("BotName")
```

From then on, all game interactions should go through game, one way or another.

Objects in the game are one of the following:

1. Game
The game, as mentioned prior contains everything under the sun related to the specific Halite game being played.
2. Map
The map contains the current turn's specific information, including all objects that consist of the map. Map metadata such as size is also included.
3. Player
The player is a representation of the Halite bot. It contains ships and an id.
4. Entity
The entity represents any objects that occupies a space on the map. This includes Ship, Planet, and Position. All are represented by id, x-coordinate, y-coordinate, radius, health, and owner. Note that Position effectively only has x-coordinate and y-coordinate as valid fields. 

None of these should be initialized by the player, but should rather be ready for access by the library (i.e.: it's all parsed for you).

## Turns

Since Halite is turn based, we must wait for the signal that the new turn has begun before issuing actions. We do that by updating our map with:

```python
game_map = game.update_map()
```

Once that is executed, our map will be updated, and all items within will be accessible. During the extend of that turn you must construct a set of commands to be sent to the Halite Game engine. Commands are constructed via a series of methods which we provide and are discussed in their respective sections. Commands are sent with: 

```python
game.send_command_queue(commands)
```

At the end of our turn a command must be sent or your bot will be kicked out. That command can technically be empty if you don't wish to move (in which case commands should be an empty list).

## Game Map

The game map contains all metadata apposite the current turn. The following information is also always available from the map:

1. id
2. width
3. height

Following are a set of methods and their explanations:

### get_me

Returns your player object. You can use this object to interface with your ships and fetch your id.

### get_player

Returns the player object for the player with the designated id. You can use this object to determine any player's metadata (including ships and ids)

### all_players

Returns a list of every player currently playing the game (including yourself)

### get_planet

Returns the planet object designated by the given id. You can use this object as a target to navigate towards or learn about the planet's information.

### all_planets

Returns the list of all (existing) planets on the board. Planets that have exploded are not in this list.

### obstacles_between

Determines whether there are obstacles between two designated entities (ships/planets/positions). If there are none, returns None/Null equivalent, otherwise will return the list of entities.

### nearby_entities_by_distance

Returns the full list of entities in the map in a dict, keyed by their distance from the source object.

## Player

The player contains a representation of the bot in the game. The player object contains both the player's local id as well as the ships the player owns. The following information is accessible:
1. id

Following are a set of methods and their explanations:

### get_ship

Returns this player's ship object designated by the given id. The ship object can be interacted with to move across the field, dock, undock, etc. It is your main form of interaction on the map.

### all_ships

Returns a list of all the ships this player contains.
