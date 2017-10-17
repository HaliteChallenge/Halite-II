---
layout: doc_page
title: ML starter bot
description: An ML starter bot from the Two Sigma Team
image: assets/images/temp/bot_1.png
content: website
sort_key: 7
---

## Bot in action

This bot is playing under [tscaptain-ml](/user/?user_id=1154) in the global leaderboard, You can check out some of its games [here](/user/?user_id=1154) or see below for a quick preview.


<div class="static-container text-center">
    <img style="width: 60%;height: auto;" src="/assets/images//tutorial-images/mlbotpreview.gif">
</div>


## Bot Strategy
It uses a neural network to try to answer the question: __Given the current situation in the game, what % of our ships should we send to each planet?__. The network produces an answer in the form of __ship x% of un-docked ships to planet A, y% of ships to planet B, etc__.

Given the percentage for each planet, we perform a simple greedy assignment to decide which ship goes where. If a planet belongs to an enemy, we go to its weakest docked ship. If not, we try to dock.

## Why this approach
In our model we don’t have to worry about different number of ships, different board sizes or symmetry. The network is equivariant to permutation (see [neural_network_test.py](https://github.com/HaliteChallenge/Halite-II/blob/master/airesources/ML-StarterBot-Python/tsmlstarterbot/neural_net.py)), which means the results depend solely on the features of the planets and not on the order they are presented to the network. This makes training much easier.

The model is trained on historical data. We look at a particular bot (usually the one that was winning a lot), and train the network to mimic its behavior. 

## Running the bot

### Prerequisites

- Python 3
- Numpy

### Training

- The bot needs a zip file with the games in json format to train on. We packed about 1000 games and made them available online for convenience.
- After you run “make”, the script will download the games and will train the model on 1000 games, serializing it to models/ directory. To create an archive you can submit to the contest, run “make submission”.

### Submission


## Next ideas to try
- Different network architectures (number of hidden units)
- Different models for different parts of the game (one model for the beginning of the game, another model for the rest of the game)
- A separate model for 4 players game
- Introduce state to the bot (take previous decisions into account)

You can read more about parameter sharing and equivariant networks [here](http://www.deeplearningbook.org/contents/convnets.html); 
