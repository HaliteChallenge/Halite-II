<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Introducing Halite</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
    <link href="style/learn.css" rel="stylesheet">
</head>
<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="row">
            <?php include 'includes/learn_sidebar.php'; ?>
            <div class="col-sm-9">
                <h1>Introducing Halite</h1>

                <p>In this tutorial, we're going to walk you through the basics of the Halite competition. We'll cover:
                <ul>
                    <li>How to submit a bot to the competition</li>
                    <li>The rules of the game</li>
                    <li>How to run games locally on your computer</li>
                </ul>
                </p>

                <h3>Halite Overview</h3>

                <p>Halite is a multiplayer turn-based strategy game played on a rectangular grid. The objective of the game is to take over the entire grid and eliminate the other players in the game.

                <p>As a human player, you don't interact with the game directly. Rather, you program a bot that will play games on your behalf. From this point of view, Halite is really a programming game.</p>

                <p>Halite continuously and randomly organizes "tournaments" between bots and uses the outcome to rank bots and create a leaderboard for players.</p>

                <p>The simplest Halite bot is just 10 lines of code, and it takes less than a minute to get on the leaderboard. Interested?</p>

                <h3>Submitting a Bot</h3>

                <p>The first thing we're going to go through is how to submit a bot to the Halite competition. The bot will be rather bad at playing the game, making only random moves, but that's okay, because what's more important is that it provides a base from which to work up to a competition-winning bot. The reason we're showing you how to submit first is that the servers take time to run games; the goal is that by the time you finish the rest of this tutorial, some results will be up for you to check on afterwards.</p>

                <p>In Halite, rankings are determined by games played on the servers. To get your bot played on the server, it needs to be submitted properly, and this means that you submit your source code to the servers. In short, to ensure successful submission ensure that your code will compile and run on Ubuntu 14.04 LTS and that you've included all relevant files in your zip. For this demonstration, though, you won't need to worry about that because we've confirmed that our starter packages will work with our servers.</p>

                <p>Let's start by heading over to the <a href="downloads.php">downloads page</a>. You'll notice a list of starter packages; this is a set of prewritten random bots that give players some boilerplate code to start off with. You're welcome to rewrite them yourself (and many people eventually do), but for now we'll just use them as they are.</p>

                <p>Next, download a package in your preferred language. If you're new to programming and don't know which to choose, we recommend Python 3, as it's the easiest to learn and use of the bunch. Then, click on <a href="https://github.com/login/oauth/authorize?scope=user:email&client_id=2b713362b2f331e1dde3">this link</a> (alternatively found at the top right of the page) to log into Halite using your GitHub account. If you don't have an account, that's OK; just head on over to <a href="https://github.com">here</a> to make one first.</p>

                <p>Once you've signed in, a button should appear near the top right entitled "Submit". Click on it and select the zipped starter package you just downloaded. Congratulations! You've now submitted your first bot to the Halite competition! While we wait for some results to come out, let's go through the actual Halite game and competition.</p>

                <h3>Halite Game Overview</h3>

                <p>Halite is played on a rectangular grid. The grid is composed of sites, each site with its predefined and fixed production value.</p>

                <p>Players own pieces, with each piece occupying a given site and having its own strength.</p>

                <!-- <img src="http://halite.io/website/assets/basic_intro_territory_labels.png" style="width:100%;height:100%"> -->

                <p>At each round, players (bots playing on behalf of players, to be precise) decide how to move the pieces they own. Valid moves are: STILL, NORTH, EAST, SOUTH, WEST.
                <ul>
                    <li>When a piece remains STILL, its strength is increased by the production value of the site it is on.</li>
                    <li>When a piece moves, it leaves behind an identical piece (same owner) but with a strength of zero.</li>
                </ul>
                Moving pieces is how players gain new pieces and expand their territory.</p>

                <p>When two or more pieces from the same player try to occupy the same site, the resultant piece gets the sum of their strengths. Note that the strength value is capped at 255.</p>

                <p>When pieces from opposing players try to occupy either the same or adjacent sites, the battle will be resolved according to the relative strengths of the pieces, as each piece decreases the Strength of every adjacent or coinciding opposing piece by its own Strength.</p>

                <p>At the end of the turn, pieces with a strength of 0 or less are removed from the game, excepting pieces with a strength of 0 which have not engaged in combat during that turn.</p>

                <p>The map is initialized by the environment to have productions and strengths. Combat with map squares works identically to combat with other players except only applies on that square; empty map squares neither apply damage to nor take damage from adjacent squares. Players should note that the map does wrap around; if a piece at the top of the map moves North, it will reappear on the bottom of the map, and pieces on the top and bottom of the map will engage in combat (provided that they are in the same column).</p>

                <p>Players are scored according to the reverse of the order in which they are destroyed. The player last standing wins, whereas the player which was destroyed first comes in last. Bots are scored relatively; in a game with many players it is far better to come in second than to come in last.</p>

                <p>The game ends if one of two conditions are met:
                <ul>
                    <li>Only one player has any pieces left.</li>
                    <li>10 * sqrt(WIDTH * HEIGHT) turns have been played. This means larger maps may take many more moves to complete than small maps.</li>
                </ul>
                The maximum number of turns is generally high enough that only the best-matched of bots will reach the turn limit; the majority of games will end before the turn limit is reached. In the event that the turn limit is reached or multiple bots are destroyed on the same turn, they are ranked based on their territory at that point in the game. If there is a tie in the amount of territory each bot possesses, the full territory integral is used as the tiebreaker, although this is a rare occurence.</p>

                <p>Whew! Seems like a lot, doesn't it? Even though they're a bit non-intuitive, after you familiarize yourselves with the rules they'll actually seem quite simple, because they are. The challenge is in building a bot which can navigate them.</p>

                <h3>Running a local game</h3>

                <p>So, we now know how to submit a bot and generally understand the rules of the game. What next?</p>

                <p>In order to work on your bot, you'll probably want to do a lot of testing in order to see which ideas work and which don't, and be able to select and combine those which do as effectively as possible. If you had to submit to the servers and wait for results every time, development would get quite tedious and slow. Luckily, you can instead run games locally on your computer as you wish, allowing you to better and more quickly evaluate your bots as you'd like. This section of the introduction will show you how.</p>

                <p>Let's start by revisiting the <a href="downloads.php">downloads page</a>. You'll notice the section entitled "Game Environment"; this contains precompiled binaries of the game environment with which to run games on your computer. This environment is indentical to the environment used on the servers, so you can be confident that you can get repeatable behaviors between local testing and submission. Additionally, if you'd like to compile an environment for yourself, you can find the source code on our <a href="https://github.com/HaliteChallenge/Halite">github repo</a>. Download or compile the appropriate environment for your OS, and put it in a clean directory to work within.</p>

                <p>The following instructions are designed to work with Python 3. If you downloaded a different starter package in an earlier part of this tutorial, please either download the Python package as well or be prepared to compile and run the language of your choice.</p>

                <P>Next, if you are using the Python starter package, please ensure that you have Python 3 installed on your computer and added to your PATH. Then, extract the contents of the starter package to your newly created directory.</p>

                <p>Open up a new terminal or command prompt and navigate to the new directory. If on windows, run the command <code>.\environment.exe -d 30 30 "python MyBot.py" "python MyBot.py"</code>; if on a Unix-based system, first run the command <code>chmod +x environment</code>, which marks the environment as executable, and then run the command <code>./environment -d 30 30 "python3 MyBot.py" "python3 MyBot.py"</code> to run the game. This will run a game between two random bots on a 30x30 random map. For more detail on how to use the environment, please visit <a href="advanced_command_line.php">this guide</a> or run it with the <code>--help</code> [COMING] flag. As the environment runs the game, it should output a stream of information about the game, ending with the final results of the game<./p>

                <p>A new file with the file extension <code>.hlt</code> should have just been created in the directory. This is a replay file, and it contains all of the data of the game in the format specified <a href="advanced_replay_file.php">here</a>. In order to visualize the game you just played locally, visit <a href="local_visualizer.php">this visualization page</a> and drag and drop your newly created file onto the page to view the game. Congratulations! You now know how to run a local game of Halite!</p>

                <h3>Conclusion</h3>

                <p>Thanks for sticking with us until now! By this time, assuming you didn't just scroll past all of the above, the servers should have had enough time to run a few games. Find yourself on the rankings, take a look at some of our other tutorials, and start work on your bot!</p>

            </div>
        </div>
    </div>


    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
