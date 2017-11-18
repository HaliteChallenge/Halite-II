# sbt additions

Two sbt-tasks are available
* `createZip`: Creates a distributable zip file containing MyBot.jar and the LANGUAGE file. This tasks doesn't need to be called manually before uploading the bot - sbt takes care of that.
* `uploadBot`: Uploads the bot using the `Halite Client tool`. Make sure the hlt command is available in your PATH (or customize the command in the `build.sbt`) and you already authenticated yourself with the halite api.

## usage
* `sbt assembly`: creates the fat jar using the sbt-assembly plugin
* `sbt createZip`: see above
* `sbt uploadBot`: see above

These tasks depend on each other, meaning that `sbt uploadBot` first creates the fat-jar, zips it and finally uploads it.
