---
layout: doc_page
title: Compare Bots Tool
toc: false
sort_key: 4
---

## Why Compare?

Halite players often compare multiple versions of their bots against each other to make sure that each subsequent bot submission is better than the previous one. While you dont have access to other players bots in your environment, its important to do some regression testing against your own test and/or previous bot versions to make sure that you have not introduced issues that might drop you in your rankings.

## Tools

Our deep learning team has provided an easy to use script to compare any 2 bots. We have a bash as well as a python version available that should cover your needs.

[Download: Halite Bots Compare Tool](https://storage.cloud.google.com/halite-assets/compare_bots.zip)

### Prerequisites

You need to have the appropriate Halite executable for your operating system available in order for these scripts to work. You can download the latest Halite executable [here](/learn-programming-challenge/downloads-and-starter-bots). 

### Usage

#### Python


#### Bash

`./game_download yyyymmdd`: You need to specify the date for which you want to download these games and the script will create a folder in your current directory and download and decompress all the files.