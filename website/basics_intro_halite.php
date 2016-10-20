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

                <p>Let's start by heading over to the <a href="http://halite.io/website/downloads.php">downloads page</a>. You'll notice a list of starter packages; this is a set of prewritten random bots that give players some boilerplate code to start off with. You're welcome to rewrite them yourself (and many people eventually do), but for now we'll just use them as they are.</p>

                <p>Next, download a package in your preferred language. If you're new to programming and don't know which to choose, we recommend Python 3, as it's the easiest to learn and use of the bunch. Then, click on <a href="https://github.com/login/oauth/authorize?scope=user:email&client_id=2b713362b2f331e1dde3">this link</a> (alternatively found at the top right of the page) to log into Halite using your GitHub account. If you don't have an account, that's OK; just head on over to <a href="https://github.com">here</a> to make one first.</p>

                <p>Once you've signed in, a button should appear near the top right entitled "Submit". Click on it and select the zipped starter package you just downloaded. Congratulations! You've now submitted your first bot to the Halite competition! While we wait for some results to come out, let's go through the actual Halite game and competition.</p>

                <h3>Halite Game Overview</h3>

                <p>Halite is played on a rectangular grid. The grid is composed of sites, each site with its predefined and fixed production value.</p>

                <p>Players own pieces, with each piece occupying a given site and having its own strength.</p>

                <p>At each round, players (bots playing on behalf of players, to be precise) decide how to move the pieces they own. Valid moves are: STILL, NORTH, EAST, SOUTH, WEST.
                <ul>
                    <li>When a piece remains STILL, its strength is increased by the production value of the site it is on.</li>
                    <li>When a piece moves, it leaves behind an identical piece (same owner) but with a strength of zero.</li>
                </ul>
                Moving pieces is how players gain new pieces and expand their territory.</p>

                <p>When two or more pieces from the same player try to occupy the same site, the resultant piece gets the sum of their strengths. Note that the strength value is capped at 255.</p>

                <p>When pieces from opposing players try to occupy either the same or adjacent sites, the battle will be resolved according to the relative strengths of the pieces, as each piece decreases the Strength of every adjacent or coinciding opposing piece by its own Strength.</p>


            </div>
        </div>
    </div>


    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
