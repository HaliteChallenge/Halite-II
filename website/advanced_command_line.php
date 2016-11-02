<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Environment Command Reference</title>

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
                <h1>Environment Command Reference</h1>

                <p>
                    The environment may be passed a number of flags, including:
                    <ul>
                        <li><code>-d</code>: allows the automatic input of the dimensions of the map. The following argument is expected to be a string containing the width and height (space-separated).</li>
                        <li><code>-q</code>: turns on quiet output. Output will take the form of:
                            <ul>
                                <li>A line containing the replay file name, a space, and the map seed.</li>
                                <li>For <code>n</code> players in the game, <code>n</code> lines like so: <code>rank playerID territoryValue strengthValue productionValue stillMovePercentage averageLatency</code></li>
                                <li>A line of space separated playerIDs of the players that timed out.</li>
                            </ul>
                        </li>
                        <li><code>-s</code>: provides the seed to the map generator. If this is not provided, it will use a time-based seed.</li>
                        <li><code>-t</code>: disables timeouts for the duration of the game.</li>
                        <li><code>-o</code>: overrides provided names of bots. This is only used on the servers; we see no reason for a bot developer to have occasion to use this flag.</li>
                    </ul>
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
