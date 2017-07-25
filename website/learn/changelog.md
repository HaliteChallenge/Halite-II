---
layout: doc_page
title: Changelog
toc: true
---

## Alpha 0.2.2

- Starter kits
  - Collision avoidance has been greatly improved in Python
    - Now `move_to` is on par with `adjust_for_collision` in C++/`adjustForCollision` in Java
  - Python has a new experimental `warp` that also performs pathfinding
- Game environment
  - Fixed a bug where simultaneous docking to a planet would make the planet unavailable for docking the entire game

## Alpha 0.2.1

- Performance improvements
  - Collision detection code has been improved (game environment runs slightly faster)
  - Crash fixed in collision with map boundary
- Visualizer
  - Can now click on ships

## Alpha 0.2

- Continuous collision detection
  - No more subdivision of turns
  - No more grid
  - Ships are now perfectly circular (radius=0.5)
  - Velocities/positions are now floating-point
    - Commands are still integral
    - Bots receive rounded values
- Balance tweaks
   - Ship damage 48 (before 128)
   
     Ships will take much longer to destroy each other (5 turns instead of 2)
   - Planet resources scaling 144 (before 100)
   - Ship resource cost 72 (before 100)
   
     Each planet now has the potential to make twice as many ships as before.
     
   - Base resource rate 8 (before 25)
   
     Production is much slower: 9 turns/ship instead of 4.
   - Additional resource rate 6 (before 15)
   
     Production scales much more slowly: to produce one ship/turn, you now need 12 docked ships instead of 6
   - Max speed 15 (before 30)
   
     It takes much longer to cross the map, making it easier to predict and react to incoming attacks.
   - Max acceleration 5 (before 10)
   
     It takes longer to reach maximum speed, and it's harder to quickly modify your velocity, making movements more predictable.
   - Drag 2 (before 3)
   
     Bots that do not use `warp` or take advantage of inertia can move only
     2/3 as fast as before, and proportionally, drag is much higher compared to max acceleration now.
   - Minor map generation tweaks
     - Planets tend to be smaller
     - Tries to place 8 planets/player (before 6)
   - No more docking contention (when two or more players try to dock to the same planet in the same turn, no ships dock and nobody may dock to that planet for that turn)
     - This is expected to be further refined
- Starter kits
  - API tweaks (will require porting)
  - Python starter kit now has built in constants
- Game coordinator
    - Generates rectangular maps (landscape orientation only)
    
      Possible side lengths remain the same
    
    - More lenient about disabling bots that error too much
    - TrueSkill tau parameter is now 0
- Visualizer
  - Keyboard controls
    - A/D/Left/Right to scrub
    - Space to play/pause
  - Less CPU intensive, especially when paused
  - Interpolates between frames for smoother movement
- Error logs
  - Friendlier and more informative messages
  
    Example for invalid input:

        ERROR: Bot #0: Received invalid character 'j'. (at character 1.)
        Input received from bot:
        jt 0 1 90jt 1 1 90jt 2 1 90
        ^

    Timeout:

        ERROR: Bot #1: Timeout reading commands for bot; select() result: 0 (max time: 30 milliseconds).
        No input received from bot.
        Skipping replay (bot errored on first turn).

## Alpha 0.1 (Initial Release)