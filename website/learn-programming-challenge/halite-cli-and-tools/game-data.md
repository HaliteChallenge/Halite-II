---
layout: doc_page
title: Game Download Tool
toc: false
sort_key: 3
---

## Game Files

All game replays played by our workers are available for download. You can use this to inspect and visualize games or use these files to train your ML bots.

There are 2 buckets where you can download these files. We are not planning to delete the replay files of gold games at this time, but we might choose to add a retention policy to the the halite-2-replays bucket.

* **Top tier games (gold and above):** [gs://halite-2-gold-replays/](https://storage.cloud.google.com/halite-2-gold-replays//)
* **Other tier games (silver and salt):** [gs://halite-2-replays](https://storage.cloud.google.com/halite-2-replays//)

## Tools

Our deep learning team has provided an easy to use script to download games from the top tier bucket for a given day. The script should work on Mac and Linux OS'es. If you need a windows version, we are looking for a community [contribution](https://github.com/HaliteChallenge/Halite-II) on this front.

[Download: Halite Games Download Tool](https://storage.cloud.google.com/halite-assets/game_download.zip)

### Prerequisites

We [zstd](http://facebook.github.io/zstd/) to compress our replay files so you need to have these zstd tools in your path for the script to work.

* Download the [latest release](https://github.com/facebook/zstd/releases) of zstd
* Unzip and run `make install` on the root folder

### Usage

`./game_download yyyymmdd`: You need to specify the date for which you want to download these games and the script will create a folder in your current directory and download and decompress all the files.