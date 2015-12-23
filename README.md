# Halite

## To Do Michael

### Website
- Allow people to upload their bot code and store position of code in database
- Setup leaderboard

### Halite Program
- Switch to pipes on Windows
- Switch to pipes on Linux
- Switch to pipes on clients
- Test networking on unix and windows
- MAKE IT SO THAT USERS CANNOT SEND GIBBERISH. IF WE CANNOT PARSE IT, THEN KILL THEIR PROCESS.
- In thee Java API Networking Class, to exit on error, we do this in one of our methods:
	System.exit(1);
	return null; // the java compiler is stupid
Is there a prettier way? The compiler makes us put a return statement eventhough we are exiting.

### Worker
- Rewrite Sandbox in C++
- When sent a request with 2 userIDs, have php program spawn c++ program which plays halite with 2 sandboxes (starts docker container which redirects program output to stdout) and returns the result of the game
- Separate compiling and running. Store compiled programs on server

### Server
- Continuously tell workers to play games and update the Trueskill rankings accordingly

## To Do Ben
- WebGL
- Change text rendering from FTGL to plain Freetype
- Have files store more data than just the map position (will be useful in future)
- Fix map system
- Talk to companies
- Fix relative scoring system
- Update rules on the Google Doc

## To Do Luca
- Make website pretty

## About
Halite is a game meant to be played by computers. Halite was created by [Benjamin Spector](https://github.com/Sydriax "Benjamin Spector") and [Michael Truell](https://github.com/truell20 "Michael Truell").

This repository contains the environment for running Halite games and replays and Starter kits for making your own AIs.
