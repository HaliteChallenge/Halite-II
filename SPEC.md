# Overview

Halite is an online programming challenge. Users write bots to play a simple, original game with the following rule set:

* INSERT RULES HERE

Users develop their bots locally using our game engine, zip and submit their source to our website when they are ready to test out their bot, and watch as their bot plays against others and is ranked on our leaderboard.

# Project Components

## Environment

## Website

Our website, halite.io, contains our leaderboard and a number of tutorials and specs.

The frontend is written in HTML, CSS, and Javascript. The Bootstrap CSS library is used for styling. The Pixi javascript library is used for our game visualizer. The JQuery library is used for DOM manipulation and for AJAX calls. No templating is used at all on the frontend. All interactions with our database are done through REST calls made through the JQuery AJAX library to our backend. 

The backend is written in PHP. MYSQL is used as its database. Apache is used as its webserver. 

The webserver on which the website is hosted also hosts the manager and is used for the storage of error logs, re


## HCE

### Manager

### Worker

## Disk Storage

## Backups

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

# Installation
