# alt-python3-halite-starter
Alternative Halite starter package for Python3

The main contribution of this alt Halite starter package is a signficantly refactored hlt.py (which also subsumes the networking.py file).  

My initial bots were RandomBot, and ImprovedBot, just like most everyone else.  After seeing how ImprovedBot worked, ideas immediately come, and it's natural to just extend that existing code.  After a couple submits to the Halite servers, I then ran into two problems using the official hlt.py code:

* It didn't seem pythonic, and I found it very difficult to work with.  I constantly confused Sites and Locations, and all the double-looping in my botcode was killing its readability.  

* It was slow, and my simple bots were timing out.  I think this was caused by all the indirect object references.

The improved features and differences with the official starter package as it relates to hlt.py are these:

* I eliminated Sites and Locations.  These have been combined into just one object: a Square.  Square is simply a namedtuple with five fields: x, y, owner, strength, production.

* The game_map is iterable.  Instead of writing:
	```python
  # No, no, no, ugh
  for x in range(gameMap.width):
      for y in range(gameMap.height):
          square = gameMap.getSite(Location(x, y))
  ```

	We can just write:
  ```python
  for square in game_map:
  ```

  and then nifty stuff like:
  ```python
  # I can't even bring myself to write the translation of this using the official hlt.py
  my_production = sum(square.production for square in game_map if square.owner == myID)
  ```

* I added a neighbors method for game_map.  You can read how it works, but instead of writing gross code like this:
  ```python
  if any(getSite(my_loc, direction).owner != myID for direction in CARDINALS):
      # do something
  ```

  It's a lot more readable (to my eye) this way:
  ```python
  if any(neighbor.owner != myID for neighbor in game_map.neighbors(square)):
      # do something
  ```

  Sometimes we just want to iterate over the neighbor squares as above, and other times we also need to know which direction each neighbor is from our square.  We can use Python's enumerate() function to accomplish this:

  ```python
  baddest_neighbor, direction = max((neighbor.strength, direction) for direction, neighbor in enumerate(game_map.neighbors(square)) if neighbor.owner != myID)
  ```


In this repo, I've included the same files as the official Halite starter package but with the botcode refactored to work with the refactored hlt.py.  I've also included Python3 translations of the "Now what?" bots created by @nmalaguti in his excellent Halite forum post http://forums.halite.io/t/so-youve-improved-the-random-bot-now-what/482   Reviewing the code for these bots should highlight the functionality of the refactored hlt.py in this alt starter package.

Official package RandomBot main logic loop:
```python3
while True:
    moves = []
    gameMap = getFrame()
    for y in range(gameMap.height):
        for x in range(gameMap.width):
            location = Location(x, y)
            if gameMap.getSite(location).owner == myID:
                moves.append(Move(location, random.choice(DIRECTIONS)))
    sendFrame(moves)
```

Alt package RandomBot main logic loop:
```python3
while True:
    game_map.get_frame()
    moves = [Move(square, random.choice(DIRECTIONS)) for square in game_map if square.owner == myID]
    hlt.send_frame(moves)
```

After working with this refactored hlt.py, I find I can express my bot-code ideas much more quickly and expressively.  And it's significantly faster.  I'm getting speed-ups of anywhere 4x to 10x over same logic written with the official starter package, and timing out is a distant memory.










