---
layout: doc_page
title: Best Practices
toc: true
---

In this guide, we will list a couple of simple, useful practices to follow when building your Halite bot.

## Using a Log File

Stdout and stdin in are used to communicate with the game environment. As such, you cannot use functions like System.out.println, print(), or std::cout. Instead, print debugging information to a log file.

## Local Bot Evaluation

Before submitting a new bot to the online leaderboard, we recommend running some games against the version of your bot that is currently on the leaderboard. If your new bot consistently wins, then put it up!

## Disabling the Timeout Flag

When debugging latency issues with your bot, it can be helpful to disable game environment timeouts. To do so, append the -t flag to your environment command, like so:
 
    ./environment -d "30 30" "python3 MyBot.py" "python3 RandomBot.py" -t

## Debugging with an IDE

There is a community contributed method for running a Halite bot from a custom debugger locally. More on this can be found [here on the forums][debugger-method]. __Warning: this method has not been tested with Halite II. Use at your own risk.__ 

[debugger-method]: http://forums.halite.io/t/running-your-halite-bot-from-a-debugger/70