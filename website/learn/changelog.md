---
layout: doc_page
title: Changelog
toc: true
---

## Alpha 2.0

- Continuous collision detection
  - No more subdivision of turns
  - No more grid
  - Velocities/positions are now floating-point
    - Commands are still integral
    - Bots receive rounded values
- Balance tweaks
   - Ship damage 48 (before 128)
   - Planet resources scaling 144 (before 100)
   - Ship resource cost 72 (before 100)
   - Base resource rate 8 (before 25)
   - Additional resource rate 6 (before 15)
   - Max speed 15 (before 30)
   - Max acceleration 5 (before 10)
   - Drag 2 (before 3)
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
  
## Alpha 1.0 (Initial Release)