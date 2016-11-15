#!/bin/bash

ocamlbuild -lib unix MyBot.native
ocamlbuild -lib unix RandomBot.native
./halite -d "30 30" "./MyBot.native" "./RandomBot.native"
