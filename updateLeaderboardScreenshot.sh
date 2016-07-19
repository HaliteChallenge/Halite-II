#!/bin/bash

xvfb-run --server-args="-screen 0, 2048x2048x24" cutycapt --url=http://halite.io/website/leaderboard.php --out=/root/Halite/leaderboard.png
