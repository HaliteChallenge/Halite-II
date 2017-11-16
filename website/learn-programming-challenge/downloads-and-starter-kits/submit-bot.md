---
layout: doc_page
title: Submit your bot
description: Overview of guidelines and best practices on how to submit bots
image: assets/images/opengraph.png
content: website
sort_key: 2
---

## Guidelines

Before submitting a bot, make sure you adhere to our guidelines, or the upload or compilation of your bot will fail.

1. You should have a `MyBot.{extension for language}` in the root folder of your zip. For Rust, this should be a `cargo.toml`
2. If you are building on top of starter kit provided by us, make sure to include the hlt folder.

## Upload your bot

* Website: You can use the [play page](/play-programming-challenge) in the Halite website to submit your bot.
* Halite Client: If you enjoy a command line experience, you can use the [Halite Client tool](/learn-programming-challenge//halite-cli-and-tools/halite-client-tools) to upload your bot.

## Most common issue
If you compress your .zip file by compressing the folder containing all the files, you will get a compilation error.

### Wrong:
``` 
submission.zip
- /Submission
|- MyBot.py
|- /hlt
```
As you can see, the submission.zip first has the folder then the files. Instead, put files in the root of the document - highlight all the files that you want to compress, not the file containing them.

### Right:
``` 
submission.zip
- MyBot.py
- /hlt
```

## Related Reading
 
 * [Customize your bot](customize-bot)
 * [Debug your bot](debug-bot)
