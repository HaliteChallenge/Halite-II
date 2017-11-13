---
layout: doc_page
title: Contest Rules
toc: false
description: Learn the rules of the game to win the Halite AI Programming Challenge.
sort_key: 0
---

Halite is a fun open-source competition aimed at helping you learn about AI.  All are welcome to play as individuals or as teams. Here are the important things to know about the competition.

*Start date:* 6am on October 23rd, 2017

*Submissions end:* 11:59pm ET on January 22nd, 2018

*Winners announced:* January 29th

Read below for details about our ranking system and other information about how we calculate winners.


## ELIGIBILITY

All it takes to play Halite II is to [create an account][login] using Github and [submit a bot][downloads] during the dates of the competition. Any user who submits a bot will appear on the leaderboard.

## TEAMS

As opposed to last year, we are encouraging playing as a team this year, with the caveat that a single person should not be submitting bots under two accounts at the same time. For example, [Two Sigma](https://www.twosigma.com){:target="_blank"}, the creator of Halite, is hosting a tournament for select university students to compete on school teams for a given period of time. For these students, we’d expect to see only one player submitting bots on behalf of the team. We understand that hackathons and events are fun ways to learn and progress. However, if we see an individual creating multiple accounts with similar bot submissions, both may be disqualified. If a player in a team wishes to play on their own, they should substantially improve their bot before submitting on their own. Play fair!

## RANKING
Rankings in Halite II are based on the outcome of organized games where bots play against each other. Bot rankings are computed using a Bayesian algorithm variant of the Glicko system, specifically using the [TrueSkill](https://www.microsoft.com/en-us/research/project/trueskill-ranking-system/){:target="_blank"} Python library with some adjustments. The game coordinator picks groups of 2 or 4 bots to compete against each other at a time, and uses TrueSkill to update the bot ranking based on the match outcome.

TrueSkill gives every user a μ (calculated player skill) and σ (the probability that TrueSkill understands a player’s skill level). Your rating is calculated as a function of these two variables. When you submit your first bot, your rating will be zero, but your bot will immediately start playing games against other bots, and your rating will adjust accordingly. You’ll start by playing bots with similar ranks to yours, within your own tier (see below). Over the course of a few days, TrueSkill will get a good sense for your bot’s skill, and your rank will stabilize.

If you submit a second bot, we do not reset your rating to zero (as we did in Halite I), and you will start from the rank of your last bot. But this doesn’t mean your rank is safe forever! If your second bot is worse than your first, your rank will fall as your new bot plays others, and of course if your second bot is better, your rank will improve.

### Why don't we reset ranks for new bot submissions?

In Halite 1, submitting a new version of a bot would reset that bot's rank. In Halite 2, we decided against this. Since we wanted to provide GPU resources, but limit them to certain top bots, resetting ranks would effectively mean that no bot would ever be able to take advantage of the GPU---since submitting a GPU-enabled bot would reset its rank and deny it access. Furthermore, for top players, this caused some volatility towards the end, as submitting a new bot would mean re-climbing the leaderboard.

## TIERS
We assign you a tier based on your current rank.  Tiers are by percentile: the top 1/512 players are considered Diamond; the next 1/256 are considered Platinum, then the next 1/128 are Gold, 1/64 are Silver, and the rest are Bronze. You will move from tier to tier as your rank changes.

## FINALS
At midnight on January 22nd, we will close submissions and let the final bots play each other for the last week of January. We will announce final winners on January 29th based on the final scores at that point in time and will freeze scores. We may also decide that Bronze level bots (or any other bot) will stop playing earlier than 29th if we need the bandwidth to play more games for top bots in the last few days. Stay tuned for updates.

## PRIZES
Prizes for this contest are bragging rights and some awesome Halite trophies and sweatshirts for top bots. In addition, Two Sigma will waive first round interviews for all users ranked Gold and above at the end of the contest. [Learn more here](two-sigma-recruiting). And we’ll be announcing the results of the competition with a link to the top players’ Github profiles and/or blogs (please do blog!).

## HACKATHONS
Throughout the competition, Two Sigma and its partners will be hosting hackathons for players to compete against smaller pools. Some examples include a High School hackathon in NYC in November and a hackathon at [Two Sigma Ventures](https://www.twosigmaventures.com/){:target="_blank"}. If you’d be interested in hosting your own hackathon or use Halite in the classroom, read our [hackathon tutorial](hackathon-rules).

## COMMUNITY POLICIES
One of the most wonderful aspects of Halite I, according to last years’ players, was our open and collaborative community on [the forums](https://forums.halite.io) and on our [Discord channel](https://discordapp.com/invite/EqW8DCB){:target="_blank"}. Please share ideas, tips, and even code for all to share. We only ask that you be courteous to your fellow players, and not share so many tricks or code that earlier players feel their effort was wasted. This is of course a gray line, so feel free to err on the side of over-sharing, and the Halite staff will help with moderation in the forums. We reserve the ability to remove any content we feel goes too far. So share, share, share and leave the tricky decisions to us.

[login]: {{ site.login_server_url }}/github
[downloads]: {{ site.baseurl }}/learn-programming-challenge/downloads-and-starter-kits
