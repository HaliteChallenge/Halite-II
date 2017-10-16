---
layout: doc_page
title: Game Servers
sort_key: 2
description: Details for hardware, compilation, game data, and GPU usage for the Halite AI Programming Challenge.
image: assets/images/temp/bot_1.png
content: website
---

## Hardware
All compilation and games are run in Google Cloud Compute Engine instances with 1 vCPU and 2.5 GB memory.

## Compute and Memory per bot
Bots are given 350 MB of RAM and equal amounts of CPU.

## Halite Environment Version
Games are always run using the most recent environment build.

## Compilation
Bot compilation is done using this [autocompile script][autocompile-script].

For more details on customizing your bot, see our guide [here](downloads-and-starter-kits/customize-bot).

The following compilers/interpreters are used:

TODO

Both Python 3.5 and 3.6 are installed. To use 3.6, you will have to explicitly use a `run.sh` file to invoke `python3.6`.

## GPU-Enabled Games

The following packages are available during __runtime__ (__not__ compile time) for GPU-enabled bots:

- cuda	8.0.61-1
- libcupti-dev:amd64	8.0.44-3

Python 3.5 Packages:
- tensorflow-gpu 1.2.1

Python 3.6 Packages:
- tensorflow-gpu 1.2.1

[autocompile-script]: https://github.com/HaliteChallenge/Halite/blob/02b8a4a8c14498ddc471039c9a453137379420c1/worker/compiler.py
