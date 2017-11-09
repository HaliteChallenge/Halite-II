---
layout: doc_page
title: Halite Client Tools
toc: false
sort_key: 9
---


# Download or install our CLI

[Download: Halite Client](https://storage.cloud.google.com/halite-content/HaliteClient.zip)

or use curl (Google auth not needed)

```
curl https://storage.googleapis.com/halite-content/HaliteClient.zip > HaliteClient.zip
```

or pip install it!

```
pip install hlt_client
```
*Note: if you choose to pip install, replace all references of `./client` below with `hlt`*

## Ready, Set, Authenticate

Before using the tool's features, you must authenticate it to act on your behalf. This is done as follows:

``` bash
./client.py auth
```

## Uploading Bots

Are you a speed demon? Do you like quickly iterating and submitting your bots? We have you covered. We have a new CLI tool for you to quickly upload bots from the terminal. To do so, simply provide the zip file as follows:

``` bash
./client.py bot -b [PATH_TO_ZIP]
```

Note that the zip file must have the MyBot executable in the topmost directory, and should likely include your "hlt/" library folder in that same directory. But don't fret: if you forgot, the CLI will tell you.

## Replays

In order to build better and stronger ML bots, we provide you a large number of replay files from all bots to build upon. To help with the download process, we provide the capability to download (and unzip) those replays built-in to the CLI. Use as follows:

``` bash
./client.py replay date -t YYYYMMDD -d [destination_folder]  # Example date: 20171023 
```

You may also download a user's specific files as follows:

``` bash
./client.py replay user -i [user_id] -l [maximum_number_of_files] -d [destination_folder]
```

Note that if you exclude -i, you'll just download your own replays.

## Gym

Want to be the very best? Like no one ever was? Well, going to the gym is the best bet, then. The gym option plays any halite bot against any other set of bots for the designated number of times in the desired map. Then it gives you the success stats for each bot (either 2 or 4 bots). Run as follows:

``` bash
./client.py gym -r [run_command_1] -r [run_command_2] -b [path_to_halite_binary] -i [number of runs] -H [map_height] -W [map_width]
# Example: ./client.py gym -r "python3 MyBot.py" -r "python3 MyBot.py" -b "halite" -i 100 -H 240 -W 160
```
