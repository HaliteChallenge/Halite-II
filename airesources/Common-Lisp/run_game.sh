#!/bin/sh

sbcl --eval "(asdf:make :mybot)" --eval "(quit)"
./halite -d "240 160" "./mybot" "./mybot"
