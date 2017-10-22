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
Bot compilation is done using this [autocompile script][autocompile-script]{:target="_blank"}.

For more details on customizing your bot, see our guide [here](downloads-and-starter-kits/customize-bot).

## Languages Installed
The following languages are preinstalled on the game servers:

- Ada
- CoffeeScript
- C
- C++
- Clojure
- Dart
- DMD
- Erlang
- Go
- Groovy
- Haskell (GHC)
- Java 8
- Julia
- Ocaml
- Pascal
- Php
- Ruby
- SBCL
- Scala
- Tcl 8.5
- MIT Scheme
- Mono (.NET)
- Node-js
- Python 3.6
- Racket
- Rust
- Octave
- Lua 5.2

## Libraries
The following libraries and packages are also preinstalled:

Python: Numpy, Scipy, Scikit-learn, Pillow, H5py, Tensorflow, Keras, Theano, Shapely, Cython, Pandas

Ruby: Bundler

## GPU-Enabled Games

The following packages are available during __runtime__ (__not__ compile time) for GPU-enabled bots:

- cuda	8.0.61-1
- libcupti-dev:amd64	8.0.44-3

Python 3.6 Packages:
- tensorflow-gpu 1.2.1

[autocompile-script]: https://github.com/HaliteChallenge/Halite/blob/02b8a4a8c14498ddc471039c9a453137379420c1/worker/compiler.py
