# Halite

## About
**Halite is still under development.** Halite is a game meant to be played by computers. Halite was created by [Benjamin Spector](https://github.com/Sydriax "Benjamin Spector") and [Michael Truell](https://github.com/truell20 "Michael Truell").

Here is a screenshot of the game visualizer (more multimedia is available in the multimedia folder):

![alt tag](https://raw.github.com/Sydriax/Halite/master/multimedia/Halite.PNG)

This repository contains the environment for running Halite games, the visualizer for replays, the starter kits for making your own AIs (Java, C++, Python), and the website that sandboxes, compiles, and runs bots and hosts leaderboards and replays.

## To Do Michael

### Website
- Setup leaderboard

### Halite Program
- Downgrade example C++ bot from C++ 11  

### Worker
- Switch to python
- Encorporate file checksums and api keys
- The worker can compile the raw bot files using the Ants Auto compile python code and our Docker sandbox
- The worker can run a game between two or more bots using our Docker sandbox

### Server
- Tell workers to compile bots when needed
- Continuously tell workers to play games
- Update rankings according to the Trueskill algorithm accordingly

## To Do Ben
- WebGL
- Change text rendering from FTGL to plain Freetype
- Have files store more data than just the map position (will be useful in future)
- Fix map system
- Talk to companies
- Fix relative scoring system
- Update rules on the Google Doc

## To Do Luca
- Make website look pretty. This will probably require incorporating Bootstrap.
