# Overview

Halite is an online programming challenge. Users write bots to play a simple, original game with the following rule set:

* INSERT RULES HERE

Users develop their bots locally using our game engine, zip and submit their source to our website when they are ready to test out their bot, and watch as their bot plays against others and is ranked on our leaderboard.

# Project Components

## Environment

## Website

The frontend of halite.io is written in HTML, CSS, and Javascript. The Bootstrap CSS library is used for styling. The Pixi javascript library is used for our game visualizer. The JQuery library is used for DOM manipulation and for AJAX calls. HTML files are classified as PHP files to allow the easy including of repeating HTML elements, for example the navigation bar. No templating is used at all on the frontend. All interactions with our backend are done through REST calls made through the JQuery AJAX library.

The backend is written in PHP. MYSQL is used as its database. Apache is used as its webserver.

The server on which the website is hosted also hosts the manager and is used for the storage of error logs, replays, and the source code of contestants.

## HCE

The "HCE"(Halite Competition Environment) is what we call the system of servers that compiles the source code of each contestant, runs games between bots, and ranks each submission. The system consists of many worker servers and one manager server. 

Worker servers query the manager server for tasks, either a compile task or a game task. If there is any bot that needs to be compiled, the manager will respond with a compile task. If there are no compile tasks, the manager will respond with a game task, which are chosen like so:

* A seed plager is chosen by picking the bot with the highest `rand()*(sigma^2)`. "Sigma" is the level of uncertainty in the Trueskill score of a bot
* An allowed rank difference `d` is computed as: `5 / rand()^0.65`
* The number of players `n` is picked at random from a range of 2-6
* The other `n-1` players are chosen at random from the players within `d`
* The width and height of the map is chosen from a range of 20-50

Once given the ID of the bot(s) that they are compiling/running, workers query the manager for the executables and source of each bot. These are removed from disk on a worker server on completion of a task.

During both compilation and runtime, bots are run within their own Docker container. Networking, RAM, CPU, and disk access is limited. 

## Database

A MySQL server is used as the database for the project. 

## Disk Storage

Replays, bot source and executables, and error logs are all currently stored on the server that hosts the website and the manager.

# Backups

Backups are made hourly from the website and the database to a server using rsync. Only the most recent data from the webserver is kept, while all versions of the database are stored.

# Monitoring

A status page located at `halite.io/website/status.php` includes the time since every worker has queried the manager, the throughput of the HCE, and general stats about our user base. Google analytics is included on the site. 

# Configuration

An INI file, titled `halite.ini` and located in the root directory of the project, is are used for all of our project configurations. Here is a sample halite.ini file: 

```
[hce]
managerURl = http://localhost/manager/
apiKey = 1234 
secretFolder = FAKE_FOLDER_NAME

[workerIPs]
FAKE_WORKER_NAME = 123.456.789.000

[email]
email = FAKE_EMAIL
password = FAKE_PASSWORD

[database]
hostname = localhost
username = root
password = pass123
name = MAIN_DB_NAME

[sso]
secret = SECRET_KEY_FORUMS
url = SINGLE_SIGN_ON_FORUMS

[forums]
apiUsername = FAKE_USERNAME
apiKey = 1234567890

[encrypt]
salt = abc123456789
```

# Server Architecture

* Insert diagram here
