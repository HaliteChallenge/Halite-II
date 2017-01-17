<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>


   <title>Game Rules</title>

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
                <h1>Game Rules</h1>

                <h3>Overview</h3>

                <p>This guide gives a detailed description of how the precise rules of the Halite game work. It is highly recommended that you have already read <a href="basics_intro_halite.php">"Introducing Halite"</a> before reading this document.</p>

                <h3>The Game</h3>

                <p>During a move, every piece you control can be given one of five moves: to move NORTH, EAST, SOUTH, WEST, or to remain STILL. When a piece move STILL for a turn, it will permanently increase its Strength by the Production of the tile it sits on.</p>

                <p>Players gain pieces by simply moving their own pieces over Sites on the map. When a piece moves off of a Site, it leaves behind a piece with an identical Owner and with a Strength of 0, in doing so expanding the size of their territory.</p>

                <p>When pieces from the same player try to occupy the same site, the resultant piece has the sum of their strengths. Strengths are capped at 255, so if two pieces with a strength of 150 were to both attempt to occupy the same square, the resultant piece would still have a strength of 255.</p>

                <div class="text-center" style="margin-bottom: 18px;"><img src="assets/combination.png" style="max-width: 500px;"></div>

                <p>When pieces from opposing players try to occupy either the same or cardinally adjacent sites (diagonals are not included), the battle will be resolved according to the relative strengths of the pieces, as each piece decreases the strength of every adjacent or coinciding opposing piece by its own strength. Pieces with a strength of 0 or less are removed from the game, excepting pieces with a strength of 0 which have not engaged in combat during that turn.</p>

                <div class="text-center" style="margin-bottom: 18px;"><img src="assets/overkill-2.png" style="max-width: 500px;"></div>

                <p>Notice how in the above example, the pieces combined prior to fighting.</p>

                <p>Because a piece damages all adjacent enemy pieces, if a piece is killed while attacking multiple pieces, it will output (often significantly) more damage than it had strength. This is referred to as "overkill" and means that bots can use their pieces tactically to their own advantage. This is shown in the below examples:</p>

                <div class="text-center" style="margin-bottom: 18px;"><img src="assets/overkill-1.png" style="max-width: 500px;"></div>

                <p>The map is initialized by the environment to have productions and strengths. Combat with map squares works identically to combat with other players except only applies on that square; empty map squares neither apply damage to nor take damage from adjacent squares.</p>

                <p>Players should note that the map does wrap around; if a piece at the top of the map moves North, it will reappear on the bottom of the map, and pieces on the top and bottom of the map will engage in combat (provided that they are in the same column).</p>

                <p>The origin of the map (the point at coordinates (0, 0)) is the north west (top left) corner of the map.</p>

                <p>Players are scored according to the reverse of the order in which they are destroyed. The player last standing wins, whereas the player which was destroyed first comes in last. Bots are scored relatively; in a game with many players it is far better to come in second than to come in last.</p>

                <p>The game ends if one of two conditions are met:
                    <ul>
                        <li>Only one player has any pieces left.</li>
                        <li>10 * sqrt(WIDTH * HEIGHT) turns have been played. Consequently, a small map may end after a few hundred turns, whereas a large map could take thousands.</li>
                    </ul>
                </p>

                <p>The maximum number of turns is generally high enough that only the best-matched of bots will reach the turn limit; the majority of games will end before the turn limit is reached. In the event that the turn limit is reached or multiple bots are destroyed on the same turn, they are ranked based on their territory at that point in the game. If there is a tie in the amount of territory each bot possesses, the full territory integral is used as the tiebreaker, although this is a rare occurence.</p>

                <h3>Bot Initialization</h3>

                <p>At the start of the game, each bot is sent some information (accessed using getInit in the starter packages):
                    <ul>
                        <li>Their own tag within the map - that is, which player they are.</li>
                        <li>The initial map state.</li>
                    </ul>
                </p>

                <p>Bots are permitted to use time at the beginning of the game to initialize. This initialization might include (but is in no way limited to) getting the initial map and player tag, identifying important, high-production regions on the map, identifying the locations of neighboring players, planning the bot's initial expansion strategy, and/or compiling a model. Once bots are done initializing (before their time is up), they should send a response (sendInit in the starter packages) with their own player name, used for human identification purposes.</p>

                <h3>Turns</h3>

                <p>After all bots have finished setting up, the environment will do the following until endgame conditions are met.</p>
                    <ol>
                        <li>Send the present gamestate - map and messages - to all players.</li>
                        <li>Receive moves from the players.</li>
                        <li>Kill bots whose responses take longer than their remaining allotted time.</li>
                        <li>Add strength to pieces which choose to remain where they are.</li>
                        <li>Simultaneously move (and combine if necessary) all player's pieces. The capping of strengths to 255 occurs here.</li>
                        <li>Simultaneously damage (and remove if damage equals or exceeds strength) all player's pieces. All pieces will output damage equivalent to their strength when starting this phase, and the damage will apply to all coinciding or adjacent enemy squares.</li>
                        <li>Check if endgame conditions have been met.</li>
                    </ol>
                <p>One should note that because all pieces damage all adjacent enemy pieces, if a piece is killed while attacking multiple pieces, it will output (often significantly) more damage than it had strength. This is referred to as "overkill" and means that bots can use their pieces tactically to their own advantage.</p>

                <h3>Timeouts</h3>

                <p>Bots are given 15 seconds to initialize and 1 second for every subsequent turn. Every bot's clock starts ticking once the environment sends its message (be it initialization or frame) to the bot and resets once the environment receives the newline character marking the end of the bot's response. If a bot's clock hits zero, it is ejected from the game and deemed to have lost. It's pieces become part of the map.</p>

                <h3>Maps</h3>

                <p>Maps are randomly generated at the start of each game. The generator does the following:
                    <ol>
                        <li>Tries to match the given width and height as closely as it can while still creating a symmetric map. Maps are guaranteed to be the given size or smaller in each dimension; never larger.</li>
                        <li>Tries to create interesting maps in which there are patches of high production squares and patches of low production squares, with fairly low noise on the small scale.</li>
                        <li>Always creates symmetric maps. Specifically, the generator generates a chunk of the map and then tesselates, reflects, and shifts it to produce the entire map.</li>
                    </ol>
                </p>

                <h3>Replay Files</h3>

                <p>Once a game ends, the environment will output a replay file. See <a href="advanced_replay_file.php">here</a> for more information regarding the replay file format.</p>

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
