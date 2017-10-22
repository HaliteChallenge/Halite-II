---
layout: doc_page
title: Game Rules FAQ
toc: false
description: We've compiled a number of the most common questions asked about the rules of Halite.
sort_key: 2
---
We've compiled a few common questions about the rules of the game and the game documentation as well as some tips about how to improve your bot. 

### 1. What's the time limit for bots timing out?
2000 milliseconds per turn

### 2. if I have an entity, how do I tell if it is a ship or planet?
 
it type(entity) == Planet:
    print("this is a planet")
elif type(entity) == Ship:
    print("this is a ship")
else

### 3. If my ship is at 100% health and another is 50% and we hit each other, do both ships get destroyed or does my ship remain at 50%?
Both are destroyed: any two ships that move to the same location on the map will explode.
Interesting strategies may arise from deliberately crashing ships into enemy ships.

### 4. Will the rules of the game change over the course of the competition?
The Two Sigma and Halite team reserves the right to make changes on game rules during the course of the game. We promise we won't do this without a very good reason that improves the competition and the fun of the game, and we will try to make any changes as backwards compatible as possible.

### More coming soon!