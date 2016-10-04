<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>IO Spec</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
</head>
<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="row">
        	<?php include 'includes/learn_sidebar.php'; ?>
            <div class="col-sm-9">
                <h1>IO Specification</h1>

                <h3>Overview</h3>

                <p>To make the interaction of bots with the Halite environment as simple as possible, bots communicate via stdin and stdout, sending strings of space-separated integers ended with a newline. Bots are expected to communicate using the format found below; any deviation from it will likely result in the bot being ejected from the game.</p>

                <p>Over the course of a game, every bot is given fifteen thousand milliseconds plus one-third of a millisecond per tile of the map per turn the game could go on. So, for example, on a 35x35 map (which can have at most 350 turns), bots have 15000 + (35 * 35 * 350 / 3) = 157916 milliseconds to send their responses. The clock starts ticking as soon as the environment finishes sending its data to the pipe and stops ticking when a newline character is received from a bot; if the clock hits 0, the bot will be ejected from the game and deemed to have lost (against all live players which are alive at that time).</p>

                <p>There are two separate formats of information that are sent between the environment and bots.
                    <ul>
                        <li>Initialization</li>
                        <li>Frame</li>
                    </ul>
                </p>

                <p>Bots are initialized only once at the start of the game, before the first frame. Following initialization, bots follow the frame format until the end of the game, when the environment will automatically terminate them.</p>


                <h3>Initialization Format</h3>

                <p>Every bot is sent the following, with each item newline-terminated:
                    <ul>
                        <li>A single integer representing their own tag within the game.</li>
                        <li>Two integers respresenting the <code>WIDTH</code> and <code>HEIGHT</code> of the map.</li>
                        <li>The production map.</li>
                        <li>The initial game map.</li>
                    </ul>
                </p>

                <p>Every bot is expected to respond with a string representing their name (newline-terminated) within three seconds.</p>

                <h3>Frame Format</h3>

                <p>Every bot is sent the the present game map (newline-terminated).

                    <p>Every bot is expected to respond with a set of moves (newline-terminated) within one second.

                        <h3>Format Specifics</h3>

                        <h4>Input Game Map Format</h4>

                        <p>The state of the map (including owner and strength values, but excluding production values) is sent in the following way:
                            <ul>
                                <li>One integer, <code>COUNTER</code>, representing the number of tiles with the same owner consecutively.</li>
                                <li>One integer, <code>OWNER</code>, representing the owner of the tiles <code>COUNTER</code> encodes.</li>
                            </ul>
                            The above repeats until the <code>COUNTER</code> total is equal to the area of the map. It fills in the map from row 1 to row <code>HEIGHT</code> and within a row from column 1 to column <code>WIDTH</code>.<br>
                            This is then followed by <code>WIDTH</code> * <code>HEIGHT</code> integers, representing the strength values of the tiles in the map. It fills in the map in the same way owner values fill in the map.
                        </p>

                        <p>Consider the following map as an example:<br>
                            <pre><code>[O=0,S=122] [O=1,S=25]  [O=1,S=18]
[O=0,S=13]  [O=0,S=45]  [O=1,S=182]
[O=2,S=255] [O=2,S=85]  [O=0,S=0]</code></pre>
                                This map would be encoded using the following string:<br>
                                1 0 2 1 2 0 1 1 2 2 1 0 122 25 18 13 45 182 255 85 0
                            </p>

                            <h4>Input Production Format</h4>

                            <p>The production values of the map are sent using <code>WIDTH</code> * <code>HEIGHT</code> integers which fill in the production values of the map from row 1 to row <code>HEIGHT</code> and within a row from column 1 to column <code>WIDTH</code></p>

                            <p>Consider the following production map as an example:<br>
                            <pre><code>[2][3][4]
[1][2][3]
[0][1][2]</code></pre><br>
                            This map would be encoded using the following string:<br>2 3 4 1 2 3 0 1 2</p>

                            <h4>Output Move Set Format</h4>

                            <p>Bots should send their moves as a list of integers in sets of 3. In each set, every first integer is the x location (starting at 0) of the site the bot desires to move, every second integer is the y location (starting at 0) of the site the bot desires to move, and every third integer is the direction the bot wishes to move the site in. The order of the sets does not matter.<br>Valid directions include:
                                <ul>
                                    <li>0 - <code>STILL</code></li>
                                    <li>1 - <code>NORTH</code></li>
                                    <li>2 - <code>EAST</code></li>
                                    <li>3 - <code>SOUTH</code></li>
                                    <li>4 - <code>WEST</code></li>
                                </ul>
                            Please note that these directions correspond most directly to screen coordinates; that is, <code>NORTH</code> decrements the y value of the site by 1 and <code>SOUTH</code> increments the value by 1. Attempts to move nonexistant or enemy pieces or to move pieces in nonexistant directions will be ignored. If multiple separate moves are issued for the same piece, the lower direction value will be preferred.</p>

                            <p>Consider the following case as an example:<br>I wish to order a piece located at (3, 4) to the East, a piece located at (4, 0) to remain <code>STILL</code>, and a piece located at (4, 5) to move <code>NORTH</code>.<br>This would be encoded with the following string:<br>3 4 2 4 0 0 4 5 1</p>

                            <p>Note: if you would like an alternate way of communicating with the environment, please see [INSERT LINK HERE] for how to communicate with the environment over sockets instead of pipes if you need to use a debugger or similar to work on your bot.</p>

                            </div>
                        </div>
                    </div>

                    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
                    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
                    <script src="script/backend.js"></script>
                    <script src="script/general.js"></script>
                </body>
                </html>
