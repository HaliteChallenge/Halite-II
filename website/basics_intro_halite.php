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
                <p>In <a href="basics_quickstart.php">the last tutorial</a>, we showed how to submit a demo bot to the leaderboard. In this short tutorial, we will go over the files included in a starter package, running a game of halite, and the rules of halite.</p>
                <h3>A look at the starter package</h3>
                <p>Your starter package should contain these files.</p>

                <table class="table">
                    <tr><th>Filename</th><th>Description</th></tr>
                    <tr><td>MyBot</td><td>Your main file. Starts containing the code for a random bot.</td></tr>
                    <tr><td>RandomBot</td><td>A random bot to test changes to your bot against.</td></tr>
                    <tr><td>runGame.sh</td><td>Script to run a game on Linux/macOS</td></tr>
                    <tr><td>runGame.bat</td><td>Script to run a game on Windows. Assumes halite.exe is in the same folder.</td></tr>
                </table> 

                <p>The file you need to change is MyBot. Regardless of what you do with your code, MyBot will be considered your main file on our game servers.</p>

                <h3>Testing your bot</h3>
                <p>To play games of Halite locally, you will need the game environment. As of this writing we support Windows, Linux and OSX platforms. You can download the game environment <a href="downloads.php">here</a>. Place the downloaded binary (halite or halite.exe) in your starter kit folder.</p>
                <p>To simulate a game, simply issue the command ./runGame.sh (Linux and macOS) or runGame.bat (Windows). This command will run a game between my MyBot and RandomBot (both are just copies of each other at this point) on a grid of size 30x30.</p>
                <p>The output should look like this and the details of the game will be stored in a file with the "hlt" extension (35538-124984302.hlt in the example below).</p>

                <pre><code>$ ./runGame.sh 
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
                <p>The console output from the game environment gives just the outcome of the game. To replay the game, drag and drop the file to <a href="local_visualizer.php">the visualizer</a>. Since the starter pack is very bad at playing Halite, your visualization will be quite dull.</p>
                <p>Here's a taste of what some very good Halite bots look like:</p>
                <div id="gameReplay" class="text-center"></div>
                <div id="gameReplayTwo" class="text-center"></div>

                <h3>Halite game rules</h3>
                <p>What do all of these pretty squares mean?</p>
                <p>Halite is played on a rectangular grid. Players own pieces on this grid. Some pieces are unowned and so belong to the map until claimed by players. Each piece has a strength value associated with it.</p>
                <p>At each turn, bots decide how to move the pieces they own. Valid moves are: STILL, NORTH, EAST, SOUTH, WEST. When a piece remains STILL, its strength is increased by the production value of the site it is on. When a piece moves, it leaves behind a piece with the same owner and a strength of zero.</p>
                <p>When two or more pieces from the same player try to occupy the same site, the resultant piece gets the sum of their strengths (this strength is capped at 255).</p>
                <p>When pieces with different owners move onto the same site or cardinally adjacent sites, the pieces are forced to fight, and each piece loses strength equal to the strength of its opponent. When a player's piece moves onto an unowned site, that piece and the unowned piece fight, and each piece loses strength equal to the strength of its opponent. </p>
                <p>When a piece loses all of its strength, it dies and is removed from the grid.</p>
                <p>For the full rules, see <a href="rules_game.php">here</a>.</p>
                
                <h3>How do we program a bot?</h3>
                <p>Move on to <a href="basics_improve_random.php">Improving the Random Bot</a>.</p>
            </div>
        </div>
        <?php include 'includes/footer.php'; ?>
    </div>


    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/lodash.js/4.17.2/lodash.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/seedrandom/2.4.0/seedrandom.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
    <script src="lib/pixi.min.js"></script>
    <script src="script/parsereplay.js"></script>
    <script src="script/visualizer.js"></script>
    <script src="script/basics_intro_halite.js"></script>
</body>
</html>
