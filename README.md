# Halite

Halite is a game meant to be played by computers. Halite was created by [Benjamin Spector](https://github.com/Sydriax "Benjamin Spector") and [Michael Truell](https://github.com/truell20 "Michael Truell").

This repository contains the environment for running Halite games and replays, a C++ starter kit for making your own AIs, and a starter AI (C++).

Here are some macro changes:
 - Revising the write-up of the rules.
 - Fix the relative score system, since it doesn't make much sense right now.
 - Adding interAI messaging (for forming alliances, coordinating attacks, etc).
 - Building a website to host playbacks of Halite games and to rank AIs against each other.
 - Finding a sponsor (or at least some form of revenue)!

Here are some micro changes:
 - Use the Compiler.py from the aichallenge git.
 - Revise the maps to use run-length encoding
 - Change from Boost ASIO sockets to C-Sockets.
 - Adding the rendering of hlt files in WebGL, so that they may be displayed from a website.
