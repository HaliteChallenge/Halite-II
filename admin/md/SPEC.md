# Halite Specification

Halite is an online programming challenge. Users write bots to play a simple, original game with the following rule set:

* Bots may move their pieces either north, south, east, or west every turn or choose to remain still.
* If a piece stays still, it gains strength equal to the production value of the current tile that it is on.
* If a piece moves onto a piece with the same owner, their strengths combine. Strength values are cut off at 255.
* A piece inflicts damage equal to its strength onto all adjacent pieces that are not maps squares but have a different owner and onto to all coinciding pieces that have a different owner (this includes map squares).
* When a piece has a strength &lt; 0, it dies.

Users develop their bots locally using our game engine, zip and submit their source to our website when they are ready to test out their bot, and watch as their bot plays against others and is ranked on our leaderboard.

**Note:** This spec details how the project currently fuctions. Changes will be made before the public launch.


### Environment
The environment is written in C++ with no dependencies. The environment starts bot processes using the start commands given to it through the command line. It then communicates with bots over stdin and stdout, sending them the map and recieving their moves. A switch to using sockets for bot communication is planned. The environment outputs a replay file, with the `hlt` extension, which may be visualized [here](http://halite.io/website/game.php).

### Website

The frontend of halite.io is written in HTML, CSS, and Javascript. The Bootstrap 3 CSS library is used for styling. The Pixi javascript library is used for our game visualizer. The JQuery library is used for DOM manipulation and for AJAX calls. HTML files are classified as PHP files to allow the easy including of repeating HTML elements (i.e. the navigation bar). No templating is used at all on the frontend. All interactions with our backend are done through REST calls made through the JQuery AJAX library.

The backend is written in PHP. Apache is used as its webserver.

The server on which the website is hosted also hosts the manager and is used for the storage of the source code of contestants.

### HCE

The "HCE"(Halite Competition Environment) is what we call the system of servers that compiles the source code of each contestant, runs games between bots, and ranks each submission. The system consists of many worker servers and one manager server. 

Worker servers query the manager server for tasks, either a compile task or a game task. If there is any bot that needs to be compiled, the manager will respond with a compile task. If there are no compile tasks, the manager will respond with a game task, which is chosen like so:

* A seed plager is chosen by picking the bot with the highest `rand()*(sigma^2)`. "Sigma" is the level of uncertainty in the Trueskill score of a bot
* An allowed rank difference `d` is computed as: `5 / rand()^0.65`
* The number of players `n` is picked at random from a range of 2-6
* The other `n-1` players are chosen at random from the players within `d`
* The width and height of the map is chosen from a range of 20-50

Once given the ID of the bot(s) that they are compiling/running, workers query the manager for the executables and source of each bot. After compilation, the resulting binary+source mix is posted to the manager. These are removed from the disk on the worker server on completion of a task.

During both compilation and runtime, bots are run within their own Docker container. Networking, RAM, CPU, and disk access is limited.

### Database

A MySQL server is used as the database for the project. 

### File Storage

Bot source and executables are currently stored on the server that hosts the website and the manager.

Error logs and replay files are hosted on AWS S3 standard storage.

### Forums

[The discourse forum software](https://www.discourse.org/) is used. User authentication is handled on our end through [discourse's sso](https://meta.discourse.org/t/official-single-sign-on-for-discourse/13045). The forums are run on their own server (2 GB of RAM, Ubuntu 14.04 64 bit). Automated emails are sent through halite@halite.io, using gmail as a service provider. We will migrate away from gmail for the public launch since they have quite a low cap on the number of messages per day.

### Backups

Backups are made hourly from the website/manager server and the database server to a separate, backup server using rsync and cron. Only the most recent data from the website/manager server data is kept, while all versions of the database data are stored.

### Admin Tools

A simple python script (`manager/commandRunner.py`) is used to run arbitrary commands on all of the workers listed in the `halite.ini` file.

A status page located at `halite.io/website/status.php` includes the time since every worker has queried the manager, the throughput of the HCE, and general stats about our user base. Google analytics is included on the site.

### Monitoring Tools

We use a series of cron jobs to alert us if one of the workers hasn't responded for a number of minutes and to alert us if there are any broken links on the site.

We plan on setting up pingdom/pagerduty to alert us of downtime.

### Configuration Files

An INI file, titled `halite.ini` and located in the root directory of the project, is used for all of our project configurations. Here is a sample halite.ini file: 

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

[aws]
accesskey = 1234556
secretaccesskey = 1234561238378
amiid = ami-2d39803a
keyname = RandomKeyName
instancetype = t2.nano
securitygroupname = security-group-name 
keyfilepath = NameOfKeyFile.pem
```

### Server Setup

Each of our servers run Ubuntu 14.04. A brief textual description of our server setup:

* One server runs the website and manager and stores bot source.
* Another server runs the database.
* Another server runs the forums.
* Another server houses backups of bots and the database.
* Another server runs a series of cron jobs that check for broken links or downed workers.
* Each worker runs is on its own server.

Below is a basic diagram of our server setup.

![Server Architecture Diagram](components.png)

