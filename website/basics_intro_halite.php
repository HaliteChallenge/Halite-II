<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Hello, Halite! (part 2)</title>

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
                <h1>Hello, Halite! (part 2)</h1>
                In part 1, we showed how to submit to the leaderboard the demo bot included with the starter kit. In this short tutorial, we will explain how to write your first bot, test it locally and submit it.
                <h3>Making changes to the demo bot</h3>
                <p>Depending of the language you chose – Python (.py), Java (.java) or C++ (.cpp) – , you should get these files when you expand your starter bot.</p>
                <!-- TODO: add in table that describes files -->
                <p>The file you need to change is MyBot. Regardless of what you do with your code, you need to have a MyBot file present for your bot to work.</p>
                <p>You can use for favorite editor to make changes. You can organize your code in multiple files. You just need to add to the zip file when you submit your bot (see next section).</p>

                <h3>Testing your bot against your previous bot</h3>
                <p>For your convenience, we provide a game simulator that lets you play locally one bot against another. As of this writing we support Windows, Linux and OSX platforms. You can dowload the game simulator binaries at https://halite.io/downloads.php . Place the binary in your starter kit folder.</p>
                <p>To simulate a game, simply runGame.sh (Linux and macOS) or runGame.bat (Windows). This comand will run a game between my MyBot and RandomBot (both are just copies of each other at this point) on a grid of size 30x30.</p>
                <p>The output should like this and the details of the game will be stored in a file with extension .hlt (35538-124984302.hlt in the example below).</p>

                <pre><code>$ . runGame.sh 
python3 MyBot.py
python3 RandomBot.py
Init Message sent to player 2.
Init Message sent to player 1.
Init Message received from player 1, MyPythonBot.
Init Message received from player 2, RandomPythonBot.
Turn 1
Turn 2
Turn 3
Turn 4
…
Turn 299
Turn 300
Map seed was 124984302
Opening a file at 35538-124984302.hlt
Player #1, MyPythonBot, came in rank #1!
Player #2, RandomPythonBot, came in rank #2!</code></pre>


                <h3>Visualizing a game</h3>
                <p>This output gives only the outcome of the game. If you want to learn more and replay the game, you can use the .hlt file.</p>
                <p>Drag and drop the file to https://halite.io/local_visualizer.php to get a visualization like this one:</p>
                <!-- TODO: add in image -->

                <h3>Halite Game Overview</h3>
                <p>What do all of these pretty squares mean?</p>
                <p>Halite is played on a rectangular grid. Players own pieces on this grid. Some pieces are unowned and so belong to the map until claimed by players. Each piece has a strength value associated with it.</p>
                <p>At each turn, bots decide how to move the pieces they own. Valid moves are: STILL, NORTH, EAST, SOUTH, WEST.</p>
                <p>When a piece remains STILL, its strength is increased by the production value of the site it is on.</p>
                <p>When a piece moves, it leaves behind a piece with the same owner and a strength of zero.</p>
                <p>When two or more pieces from the same player try to occupy the same site, the resultant piece gets the sum of their strengths (this strength is capped at 255).</p>
                <p>When pieces with different owners move onto the same site or cardinally adjacent sites, the pieces are forced to fight, and each piece loses strength equal to the strength of its opponent.</p>  
                <p>When a player's piece moves onto an unowned site, that piece and the unowned piece fight, and each piece loses strength equal to the strength of its opponent. </p>
                <p>When a piece loses all of its strength, it dies and is removed from the grid.</p>

                <h3>Submitting your new bot</h3>
                <p>For your submission, you need to zip the following files: MyBot.▢, hlt.▢, networking.▢, and all of the other files that MyBot.▢ may depend on.</p>
                <p>Once you have put all these files into an archive, just submit that zip using the Submit link.</p>
                <p>You should receive an email notification about your submission.</p>
                <p>After a few minutes, you should start seeing the new rankings for your newly submitted bot.</p>

            </div>
        </div>
    </div>


    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
