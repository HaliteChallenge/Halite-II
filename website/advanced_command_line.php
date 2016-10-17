<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Tools Spec</title>

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
                <h1>Environment Reference</h1>

                <p>
                    The environment may be passed a number of flags, including:
                    <ul>
                        <li><code>-d</code>: allows the automatic input of the dimensions of the map. The following two arguments are expected to be width and height.</li>
                            <li><code>-q</code>: turns on quiet output. Output will take the form of:
                                <ul>
                                    <li>A line containing the replay file name, a space, and the map seed.</li>
                                    <li>For <code>n</code> players in the game, <code>n</code> lines like so: <code>rank playerID territoryValue strengthValue productionValue stillMovePercentage averageLatency</code></li>
                                    <li>A line of space separated playerIDs of the players that timed out.</li>
                                </ul>
                            </li>
                            <li><code>-o</code>: allows the overriding of names. Once all of the flags and their arguments have been removed from the list of arguments, this will assume that every second argument is an override name. For example, <code>"python3 MyBot.py" -q -o -d 30 30 "This is a python bot!" "cd somedirectory; java MyBot" -w "This is a java bot!"</code> is a perfectly valid set of arguments.</li>
                            <li><code>-s</code>: provides the seed to the map generator. If this is not provided, it will use a time-based seed.</li>
                            <li><code>-w</code>: after the current game ends, the environment will start a <code>visualizer</code> binary with the current game if a <code>visualizer</code> binary exists in the current directory.</li>
                            <li><code>-t</code>: disables timeouts for the duration of the game.</li>
                        </ul>

                        Flags must be prefixed by their own dash (<code>-</code>) and must be separated by other flags/arguements by a space. They may <b>not</b> be combined like so: <code>-qst</code>.
                </p>

            </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
