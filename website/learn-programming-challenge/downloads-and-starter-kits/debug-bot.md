---
layout: doc_page
title: Debug your Bot
description: Overview of how to debug common issues with bots including compilation and timeout failures.
image: assets/images/temp/bot_1.png
content: website
sort_key: 4
---
The are primarily 3 different kinds of issues you will run into after your bot submission.

## Compilation failures

If you bot fails compilation, you will get an email to that effect. The most common issues that cause compilation failures are given below.

1. **Incorrect bot archive structure:** If you don't adhere to the guidelines specified in our [submit a bot guide](submit-bot) guide, your bot will fail to compile and you will get an email to that effect. Once you fix up your bot submission structure, this issue should resolve automatically.

2. **Infrastructure Issues:** If you are submitting a bot in a unsupported language or trying to use a library/dependency that is not pre-installed on our servers, you will have to either add a custom `install.sh` and `run.sh` script to add these dependencies, and start your bot with a custom runtime. You can also reach out to the [Halite Team](mailto:halite@halite.io) to add first class support for your bot. To read more about customizing a bot, checkout out our guide [here](customize-bot).

3. **Compilation errors in your code:** Make sure that you can compile your bot locally before submitting it to our game servers. If the bot compiles on your local machine but fails in our servers, it will be mostly due to the issues enumerated in 2).

## Timeout failures

Your bot will be killed if it exceeds the following time limits during game execution, and a timeout email will be sent with a link to the log. You can also access these logs if you go to your [user profile](/user/?me). We will disable bots that show an excessive number of timeout failures to make sure that other competitors play more challenging games. 

* **Install Time:** Prior to game start you have `10 mins` to install any dependencies.
* **Initialization Time:** Prior to the first turn, you have `30 secs` to do any game specific adjustments before the game starts.
* **Per Turn Time:** Each player has at `max 2 seconds` per turn to respond with a command to the engine, if your bot exceeds this time limit, your bot is killed.

### Timeouts in the Python Starter Bot

If you are using the Navigation function provided in the Python Starter kit, there is a chance that your bots will run into timeout issues in games with a large number of ships and planets. One of the first things you need to do is optimize your path finding in Python to get rid of these timeout issues.

You can also choose to implement a timer in your code that calculates the time spent in execution, so you can determine when you are about to exceed the time limit, and short circuit your commands to prevent your bot from getting killed.

## In game failures

Another class of failures is errors during game execution. 

* **Incorrect commands:** The most common reason for this is are custom functions that you might have written to send moves to the Game engine. These commands might be formatted incorrectly or contain characters that are not allowed by the game engine.

* **Out of memory**: Bots have a specific memory limit and if they exceed this limit, they are killed. To read more about resource limits for bots, see our guide [here](/learn-programming-challenge/other-resources/game-servers).

The easiest way to debug these issues is to look into the game logs. More information about game logs is available in the below section.

## Understanding Game Logs

When your bot times out or errors on our game servers, we save and display a log file with debugging information including the time your bot took each turn, its output each turn, and its final output from stdout and stderr.

To find these log files, visit your [profile](/user/?me) or check the email that is sent when your bot is killed due to timeout or errors during game execution. Just click the download log button in the Logs section to grab your error log for a game.