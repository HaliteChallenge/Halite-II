---
layout: doc_page
title: Design Principles Underlying Halite II
toc: false
sort_key: 1
description: This is a paper written by Benjamin Spector and Michael Truell and the Halite II Team about the concepts used in the design of the Halite II game.
image: assets/images/opengraph.png
content: website
---

### This is a paper written by Benjamin Spector and Michael Truell and the Halite II Team.
### October 2017

## 1. Introduction
Halite is a programming game where players design and program smart bots that battle head-to-head with a clear objective.
- Players of the original Halite[^1], launched in 2016, had as their goal to take over the largest share of a virtual grid.
- Players of Halite II[^2] have a similar goal, which is to occupy all the planets, be the sole survivor, produce the most ships, and/or do the most battle damage. 

Each game differs quite a bit in specifics, but architecturally, they are quite similar. In particular, each allows a bot to be developed locally on a player's personal computer and then uploaded to the Halite environment, where the bot will be compiled and then executed against other bots. Each game has strong visuals to allow people to watch their bot play in relationship to the world around it. And, each game has a leaderboard so that players can enjoy the competition with other players.

Halite was developed in keeping with the Effective Programming Competition Framework (EPCoF) described in our recent paper[^3]. EPCoF is based on the authors' experiences in developing, testing, and refining their approaches to these types of programming competitions. The Waterloo/Google Ants AI Competition[^4] and Togelius' paper[^5] also provided motivation and guidance to the authors.

The original Halite was very successful, and the authors hope Halite II will be even more so. 

## 2. The Framework Underlying Halite

This section summarizes the framework from[^3] as it applies to Halite. This includes both the principles underlying a good programming game, as well as the principles for developing the surrounding programming competition environment. 

### 2.1 Principles of Programming Game Design

The principles for the design of the programming game are as follows:

1. Halite should be simple, intuitive, and easy to start to play. Few programmers will spend hours learning complex rules and setting up an environment.
2. Halite should be visually appealing in order to draw in players to minimize the bounce rate from the website. Making games easy to view and understand also makes it more straightforward for competitors to view games and better their bots, which improves the user experience.
3. Halite games should be computationally inexpensive, since Halite may be played by very many users. 
4. Halite should be as difficult as possible to solve perfectly. Little can kill a competition more thoroughly than a player discovering a simple dominant strategy.
5. Halite should always have a path forward for users to improve their bot so that they stay engaged. Whether a user is just beginning to set up a local game environment or vying for the leaderboard's diamond slots, the user must feel there is always something to be made better about their bot. Concretely, this means that Halite’s strategy must be many-faceted, so that once a player had optimized an aspect of their bot as best as they can, they can come back to it later and turn to the next one.


### 2.2 Principles of Programming Competition Construction

The principles for the design of the actual competition environment are as follows:

- The Halite competition environment should be beginner-friendly. Just as Halite must be simple to learn, the platform in which Halite is programmed, debugged, and run must be simple to use.
- The competition platform must be secure to minimize the possibility of abuse. This is challenging because implementations will typically allow players to upload executable logic into a shared environment.
- The competition platform must easily scale with the number of contestants.
- The competition should provide real-time standings of contestants in order to show players their progress and encourage their continued interest.
- The competition platform should encourage collaboration and community. Collaboration can take many forms but due thought needs to be given to forums, social media, and the appropriate use of shared code repositories. 
- The competition must allow users to easily develop and test their bots. Speed of development, turn-around time in debugging, and flexibility of programming language use are all important objectives.


## 3. About Halite II

Halite II started with the above framework as well as the goal of leveraging much of the Halite game internals from the first competition. The Halite II team wanted the game to be sufficiently similar to the previous year's game so that user community would find it evolutionary, yet sufficiently different that the community would feel challenged. In addition, the team wanted new and very compelling visuals that would attract an even larger audience. Like in the previous game, there was considerable iteration during the design, and the game remains open source in a GitHub Repository[^6]. Therefore, the users can augment the game during the competition, for example, with new or more efficient language interfaces, and the code base can inspire other competitions in the future. 

### 3.1 The Halite II Game


Halite II is a turn-based strategy game in which two or four players participate. The rules which are described in [Halite II Rules](https://2017.halite.io/learn-programming-challenge/basic-game-rules/) and summarized below:

Each player initially has a fleet of three ships. Players can move ships around a board using three commands:

- **Movement Commands:** Ships can be commanded to move with a given angle and a velocity.
- **Docking Commands:** Ships can "dock" to a planet.
- **Undocking Commands:** Ships can "undock" from a planet.

Ships may be destroyed through battle or collision with each other and planets; and ships have an initial health and lose health as they battle.

At the start of a game, all planets are neutral and players own none of them. The sizes and locations of planets are different in each game. When a player docks on a planet, the
player "occupies" that planet and the planet produces new ships for the player. Only ships from one team can dock on a particular planet at one time, though one team can dock ships
simultaneously on multiple planets. A planet’s size determines the number of ships that can be docked on it at once, and a planet's production linearly increases relative to the number of docked ships.
Finally, planets can be destroyed through collisions with ships.

A player wins when the player is (i) the sole survivor or (ii) has occupied all planets. If time runs out and neither of these conditions are met, tiebreaker rules apply: First, the surviving team which has produced the most ships wins. If there is still a tie, the team that has done the most battle damage (destroying ships with other ships through attacks or collision) wins.
If even after this rule there is still a tie, a random winner will be chosen, but this outcome has a very low probability of occurring.


<div class="static-container text-center">
    <br>
    <img style="width: 80%;height: auto;" src="/assets/images/tutorial-images/HaliteII.png" alt="Halite II photo">
</div>
<br>

*Figure 1: This figure shows a mature, four-player game with many ships docked on planets. Ships are triangles. The colored circles are planets and have assumed the color of the docked ship(s). One or more brackets on the planets indicate ship(s) are docked.*

Even more detailed rules are available in the [Halite II Detailed Rules](/learn-programming-challenge/basic-game-rules/game-rules-deep-dive).

### 3.2 Influence of EPCoF

While as of this writing the programming game competition is only just about to begin, Halite II would seem to satisfy the EPCoF requirements outlined above.

#### 3.2.1 Principles of Programming Game Design

First, while more complex than the first version, Halite II remains a simple game. For example, programmers have very few commands they can issue. Second, Halite II follows good principles for visual design, and the games are easy to watch and understand. Third, Halite II is fast to compute. Fourth, we think (and hope!) the game is essentially unsolvable due to the variety of maps and the branching factor inherent in so many independently movable ships. Last, Halite encourages iterative bot improvement.  Anecdotally, at the end of the Halite II beta, all players confirmed that they had plenty of additional strategies to continue to improve their bots.

#### 3.2.2 Principles of Programming Competition Construction

First, the competition environment is easy to get started with for multiple reasons, including the availability of a collection of starter bots. Second, the competition environment is secure, due to the use of Google Cloud VMs, Linux cgroups, and restrictions on network communication. Third, the environment is designed to scale by automatically starting worker servers as the number of players grows.  Fourth, the leaderboard provides flexible real-time standings of contestants and has been considerably enhanced since the first competition to allow greater customization of views. Fifth, the game encourages collaboration due to its open source basis, Discourse Forums, and the players' use of a  Discord channel. Last, with support for multiple programming languages, desktop, and cloud-based execution, developers can easily develop and refine their bots. 

## 4. Discussion

This paper has provided some background on the design framework underlying the Halite II Competition. Experience with the competition will tell us how well we have designed the game and correctly predicted its compliance with EPCoF. With experience from the game competition, we also hope to enhance this piece and make it closer in detail to our companion piece on the first Halite Competition[^3].

## Acknowledgements

The authors would like to thank all who were involved with the first Halite Challenge, and, in particular, acknowledge the help of David Li, Jaques Clapauch, Harikrishna Menon, and Julia Kastner in the development of Halite II.

## About the Authors

Benjamin Spector and Michael Truell are presently at the Horace Mann School in Bronx, NY. David Li is presently a student at Cornell. Jaques Claupach, Harikrishna Menon, and Julia Kastner are employed by or consulting for Two Sigma Investments.  Thanks to Nikki-Ho-Shing for the final edits to this.


## References

[^1]: "About Halite." <https://2016.halite.io/about.php>, 2016.
[^2]: “About Halite II." <https://2017.halite.io/about>, 2017.
[^3]: B. Spector and M. Truell, “Considerations in the design of turn-based programming games.” <https://arxiv.org/pdf/1710.07738.pdf>, October 2017.
[^4]: "Ants AI Competition." <http://ants.aichallenge.org>, 2011.
[^5]: J. Togelius, “How to run a successful game-based AI competition," IEEE Transactions on Computational Intelligence and AI in Games, vol. 8, no. 1, pp. 95–100, 2016.
[^6]: “Halite II Competition Repository." <https://github.com/HaliteChallenge/Halite-II>, 2017.
