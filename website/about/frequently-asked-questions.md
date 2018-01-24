---
layout: static_page
title: Frequently Asked Questions
short_title: FAQ
permalink: /about/frequently-asked-questions
toc: false
description: Answers to frequent questions about the Halite AI programming challenge.
---

## General Questions

### What is Halite II?

Halite II (the game) is a multiplayer turn-based strategy game played on a virtual rectangular board; the objective of the game is to be the last team standing by mining planets for Halite and destroying all opponents.

Halite (the tournament) is an online programming competition where human coders write bots that play the Halite II game against each other.

### Where does the game come from? Who invented it?

Halite I was created by Benjamin Spector and Michael Truell during a summer internship at [Two Sigma](https://www.twosigma.com){:target="_blank"} in 2016. The game format is inspired by the 2011 Ants AI Challenge sponsored by Google. Halite II was created by Benjamin Spector, Michael Truell, David Li, Jaques Clapauch, Julia Kastner, and Harikrishna Menon in 2017 at Two Sigma.

### Why did you invent it?

Halite’s creators heard about the Ants AI Challenge a few years too late :-(
They couldn’t find any other competitions quite like it, so they decided to create one. Halite I was born as an internal competition within [Two Sigma](https://www.twosigma.com){:target="_blank"}.

Due to the success of Halite I, Two Sigma decided to create a new iteration of Halite I and re-host the game with new rules, challenges, and a brand new website.

### How are rankings computed?

Rankings are based on the outcome of organized games where bots play against each other. A good analogy is the Elo rating system used for chess.

Bot rankings are computed using a Bayesian algorithm variant of the Glicko system, specifically using the TrueSkill Python library with some adjustments. The game coordinator picks groups of 2 or 4 bots to compete against each other at a time, and uses TrueSkill to update the bot ranking based on the match outcome. In Halite I, when a player submitted a bot, their rating was completely reset, for Halite II, we only reduce the rating by a constant factor and not to zero.

Tiers are based on the percentile rank. The top 1/512 players are considered Diamond;the next 1/256 are considered Platinum, then the next 1/128 are Gold, 1/64 are Silver, and the rest are Bronze.

### How are the winners decided?

Winners are simply the highest ranked players on the leaderboard at the end of the competition. Last submissions are due January 22nd, and bots will run through the following week to get significant games played to compute ratings. So, submit early and often!

### Is there a prize for the winners?

Pride! Bragging rights! Appreciation from the TS team! 

### What information do you store about me?

We store the email, username, and unique identifier that Github provides when you login to the halite.io website via Github OAuth as well as any information you provide in your user profile. You can learn more in our [privacy policy][privacy].

### How do you determine my organization?

When you create an account we ask you specify that you are a professional or student and potentially ask you to add your email address that contains the domain of your organization. For example, as a Two Sigma employee, you would enter an email address ending in 'twosigma.com'. If your email domain is successfully matched with an organization, you will get a verification email. If your organization cannot be matched, you can send us your organization information to add to our list and try again later. You can add an organization anytime from your user profile (once feature is done). 

For the internal Two Sigma Beta, we encourage you to add your TS account!

## Bot Programming

### Do I need to be a programmer to play the game?

Yes. Halite is a programming competition. You need to program a bot that will play the game in the Halite tournament.

However, you definitely don’t have to be a very good programmer to play Halite effectively. Success is more about coming up with a good strategy to play the game than coding this strategy expertly.

### What languages does the game support?

Any and all! If the language can read from stdin and print to stdout, we can support it.

We provide out-of-the-box starter packages for the following languages: Python, Java, and C++. [Download one here][downloads] We’re counting on the community to add support for as many languages as people want. Visit [this page] for more information on writing your own starter package and the protocol used by the game environment to talk to your bot.

### How do I submit my bot?
To submit your bot, you'll first need to zip your source code. Then, after signing in, click the "Submit" button on the top-right part of the page. Learn more about [getting started here][learn].

### My bot keeps getting ejected because my <framework/library/toolkit> is printing things <to stdout/stderr>!

Halite reads commands from your bot's stdout and stderr, so anything else written there will get interpreted as a command. Unfortunately, anything that is *not* a command will cause the game to eject you for writing badly formatted commands.

We recommend you suppress this output. Most frameworks allow you to change the verbosity, or change where output is redirected to. In some cases (e.g. Keras), you may need to monkeypatch stderr/stdout temporarily to another location when importing/using them.

[privacy]: {{ site.baseurl }}/about/privacy
[learn]: {{ site.baseurl }}/learn-programming-challenge
[downloads]: {{ site.baseurl }}/learn-programming-challenge/downloads-and-starter-kits
