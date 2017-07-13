---
layout: doc_page
title: Frequently Asked Questions
short_title: FAQ
toc: true
---

## General Questions

### What is Halite II?

Halite II (the game) is a multiplayer turn-based strategy game played on a virtual rectangular board; the objective of the game is to be the last team standing by mining planets for Halite and destroying all opponents.

Halite (the tournament) is an online programming competition where human coders write bots that play the Halite II game against each other.

### Where does the game come from? Who invented it?

Halite I was created by Benjamin Spector and Michael Truell during a summer internship at Two Sigma in 2016. The game format is inspired by the 2011 Ants AI Challenge sponsored by Google. Halite II was created by David Li and Jaques Clapauch in 2017 at Two Sigma, with assistance from Ben and Michael.

### Why did you invent it?

Halite’s creators heard about the Ants AI Challenge a few years too late :-(
They couldn’t find any other competitions quite like it, so they decided to create one.
Due to the success of Halite I, Two Sigma decided to create a new iteration on Halite I and re-host the game.

### How are rankings computed for the private alpha?

Rankings are based on the outcome of organized games where bots play against each other. A good analogy is the Elo rating systemused for chess.

Bot rankings are computed using a Bayesian algorithm variant of the Glicko system, specifically using the TrueSkill Python library available here. The game coordinator picks groups of 2 or 4 bots to compete against each other at a time, and uses TrueSkill to update the bot ranking based on the match outcome.

Tiers are based on the percentile rank. The top 1/512 players are considered Platinum; the next 1/256 are considered Diamond, then the next 1/128 are Gold, 1/64 are Silver, and the rest are Bronze.

### How are the winners decided?

Winners are simply the highest ranked players on the leaderboard at the end of the competition. So, submit early and often!

### Is there a prize for the winners?

Pride! Bragging rights! Appreciation from the TS team! For the internal Alpha there will be no prizes but we really appreciate your help.

### What information do you store about me?

We store the email, username, and unique identifier that Github provides when you login to the halite.io website via Github OAuth as well as any information you provide in your user profile. You can learn more in our [privacy policy][privacy].

### How do you determine my organization?

For the internal alpha, we check to see if the domain of the email you added to your profile matches Two Sigma. If so, you will get a verification email. If not, you won’t be able to play in the closed alpha. So please make sure you enter your Two Sigma email correctly while signing up!

## Bot Programming

### Do I need to be a programmer to play the game?

Yes. Halite is a programming competition. You need to program a bot that will play the game in the Halite tournament.
However, you definitely don’t have to be a very good programmer to play Halite effectively. Success is more about coming up with a good strategy to play the game than coding this strategy expertly.

### What languages does the game support?

Any and all! If the language can read from stdin and print to stdout, we can support it.

We provide out-of-the-box starter packages for the following languages: Python, Java, and C++. We’re counting on the community to add support for as many languages as people want. Visit [this page][own-bot] for more information on writing your own starter package and the protocol used by the game environment to talk to your bot.

### How do I submit my bot?
To submit your bot, you'll first need to zip your source code. Then, after signing in, click the "Submit" button on the top-right part of the page. Learn more about [getting started here][learn].

[privacy]: {{ site.baseurl }}/privacy
[own-bot]: #
[learn]: {{ site.baseurl }}/learn
