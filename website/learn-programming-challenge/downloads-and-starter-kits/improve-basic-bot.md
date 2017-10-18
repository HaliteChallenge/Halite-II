---
layout: doc_page
title: Improve your basic bot
description: Tutorial to learn how to improve a basic Halite AI bot with a few heuristics as an easy way to get started playing in the Halite AI competition.
image: assets/images/temp/bot_1.png
content: website
sort_key: 6
---

In this tutorial, we’ll add a couple heuristics to the basic bot. This will hopefully help you understand Halite II better and set you on your way to dominating the universe.

## Prerequisites for Tutorial

Make sure you have read our [basic instructions]({{ site.baseurl }}/learn-programming-challenge/) and followed the steps to download your bot. Also, make sure you [understand the basic bot structure](understand_your_bot.md)
Now open the `MyBot.py` file in your favorite editor and let’s get started!

## Improving the bot

### Go to the closest planets first

We can sort the planets by distance before iterating over them. The faster we can get to a planet and dock, the faster we'll get new ships to command.

```python
planets = game_map.planets.values()
sort_key = lambda planet: hlt.distance(ship, planet)
for planet in sorted(planets, key=sort_key):
```
