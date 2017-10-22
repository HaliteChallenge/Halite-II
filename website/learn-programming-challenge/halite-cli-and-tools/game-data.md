---
layout: doc_page
title: Game Data
toc: false
sort_key: 3
---

## Game Files

All game replays played by our workers are available for download. You can use this to inspect and visualize games or use these files to train your ML bots.

There are 2 buckets where you can download these files. We are not planning to delete the replay files of gold games at this time, but we might choose to add a retention policy to the the halite-2-replays bucket.

* **Top tier games (gold and above):** [gs://halite-2-gold-replays/](https://storage.cloud.google.com/halite-2-gold-replays//)
* **Other tier games (silver and Bronze):** [gs://halite-2-replays](https://storage.cloud.google.com/halite-2-replays//)

## Tools

In order to expedite your learning process, we provide you a very useful CLI to help you get started. With it you can automatically download bots as desired. You can find it [here](halite-client-tools.md).
