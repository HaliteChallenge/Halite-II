---
layout: doc_page
title: Learn How to Get Started with Halite
breadcrumb: Learn
toc: false
menu: learn
description: Welcome to the Halite II competition! Learn how to quickly get started downloading, building, and submitting an AI bot.
mount_view: Onboarding
additional_js: ["FileSaver.min", "jszip.min", "jszip-utils.min", "editorBuild/code_edit/built-codeEdit.min"]
additional_css: ["built-codeEdit", "dark-codeEdit"]
---
<script>
  var site_downloads = {{ site.data.downloads | jsonify }};
</script>

<div id="onboarding-container"></div>

Welcome to the Halite II competition!  If you’re here, that means you’re excited to make your first bot and get on the Halite II beta leaderboard. Let's get you set up!

## Getting started

### 1) LOG IN

First, you need to [log in via Github][login] and create a user profile.

### 2) WATCH A FEW GAMES

Get a feel for how to play. Here's one of our favorites:

<img src="/assets/images/gifs/Oct-16-2017%2015-23-13%20intro.gif" alt="First Halite game video">

[Watch the whole game here.][david game]

To find more games, check out some user profiles, where you'll find recently played games.

You might also want to [browse the game rules][game rules], but it helps to watch some videos first.  

### 3) DOWNLOAD A BOT

Now it's time to [download the game environment and starter kit][downloads] for your platform and language of choice.

The game environment and starter kit are bundled together, so you should be all set.

### 4) SUBMIT THE STARTER BOT

A great way to get going with the game is to just go ahead and [submit][play] the starter bot you downloaded and then going to your [user profile][profile] to see how you've done.

We've created a lot of other documents for you to learn how to play Halite. If you've made it through all these steps, a great next resource is our guide to [improving the basic bot][improve bot].

### May the best bot win!


[login]: {{ site.login_server_url }}/github
[downloads]: {{ site.baseurl }}/learn-programming-challenge/downloads-and-starter-kits
[contact]: {{ site.baseurl }}/about/contact-us
[play]: {{ site.baseurl }}/play-programming-challenge
[submit]: {{ site.baseurl }}/learn-programming-challenge/downloads-and-starter-kits/submit
[leaderboard]: {{ site.baseurl }}/programming-competition-leaderboard
[game rules]: {{ site.baseurl }}/learn-programming-challenge/basic-game-rules
[profile]: /user/?me
[improve bot]: /learn-programming-challenge/downloads-and-starter-kits/improve-basic-bot
[david game]: /play/?game_id=1658857&replay_class=0&replay_name=replay-20171016-183304%2B0000--998929535-240-160-1508178764
