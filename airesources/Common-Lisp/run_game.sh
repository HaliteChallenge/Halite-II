#!/bin/sh

sbcl --batch "(asdf:make :mybot)"
./halite -d "240 160" "./mybot" "./mybot"
