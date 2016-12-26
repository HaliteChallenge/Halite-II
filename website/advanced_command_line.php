<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Halite Environment CLI</title>

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
                <h1>Halite Environment CLI</h1>

                <p>The Halite environment is responsible for running games between bots and outputting appropriate data and files upon the end of a game. The downloadable version is the same version used on the servers.</p>

                <p>
                    It may be passed a number of flags, including:
                    <ul>
                        <li><code>-d</code>: allows the automatic input of the dimensions of the map. The following argument is expected to be a string containing the width and height (space-separated).</li>
                        <li><code>-t</code>: disables timeouts for the duration of the game.</li>
                        <li><code>-q</code>: turns on quiet output. Output will take the form of:
                            <ul>
                                <li>Unless the <code>-o</code> option is also specified, lines with the command used to run each bot (one bot per line).</li>
                                <li>A line showing the actual map size used.</li>
                                <li>A line containing the replay file name, a space, and the map seed.</li>
                                <li>For <code>n</code> players in the game, <code>n</code> lines like so: <code>playerID rank lastFrameAlive</code></li>
                                <li>A line of space separated playerIDs of the players that timed out.</li>
                                <li>A line of space separated timeout log filenames.</li>
                            </ul>
                        </li>
                        <li><code>-s</code>: provides the seed to the map generator. If this is not provided, it will use a time-based seed.</li>
                    </ul>
                </p>

                <h3>Examples</h3>
                <p>To run your bot against itself on a 40 by 40 map with no timeouts, run: 
                    <ul>
                        <li>Linux/macOS: <code>./halite -d “40 40” -t “python3 MyBot.py” “python3 MyBot.py”</code></li>
                        <li>Windows: <code>.\halite.exe -d “40 40” -t “python3 MyBot.py” “python3 MyBot.py”</code></li>
                    </ul>
                </p>


                <p>To run your python bot against a java bot (assuming it’s been compiled) on a 25 by 25 map with a predefined seed (2168), run:
                    <ul>
                        <li>Linux/macOS: <code>./halite -d “25 25” -s 2168 “python3 PythonBot.py” “java JavaBot”</code></li>
                        <li>Windows: <code>.\halite.exe -d “25 25” -s 2168 “python3 PythonBot.py” “java JavaBot”</code></li>
                    </ul>
                </p>
            </div>
        </div>
        <?php include 'includes/footer.php'; ?>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
