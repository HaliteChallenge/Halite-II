<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Writing Your Own Starter Package</title>

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
                <h1>Writing Your Own Starter Package</h1>

                <h2>API Design</h2>

                <p>Please try to generally adhere to the API used in the Java, Python, and C++. This is as follows:
                <ul>
                    <li>Global Functions
                        <ul>
                            <li>getInit</li>
                            <li>sendInit</li>
                            <li>getFrame</li>
                            <li>sendFrame</li>
                        </ul>
                    </li>
                    <li>Classes/Structs
                        <ul>
                            <li>Map</li>
                            <li>Location</li>
                            <li>Site</li>
                            <li>Move</li>
                        </ul>
                    </li>
                    <li>Constants
                        <ul>
                            <li>the directions - NORTH, EAST, SOUTH, WEST, STILL</li>
                        </ul>
                    </li>
                </ul>
                <p>Of course, if changes to this API will make the starter package fit more nicely into your language, feel free to make them. A Java API will not translate directly into Lisp nicely.</p>

                <h2>Networking Overview</h2>

                <p>Bots communicate with the environment via stdin and stdout using series of space separated integers. There are two stages of communication - initialization and turn formats, and these are detailed below.</p>

                <h3>Initialization</h3>

                <p>At the beginning of the game, bot is sent the following, with each item newline-terminated:
                <ol>
                    <li>A single integer representing their own tag within the game.</li>
                    <li>Two integers representing the <code>WIDTH</code> and <code>HEIGHT</code> of the map.</li>
                    <li>The production map.</li>
                    <li>The initial game map.</li>
                </ol>

                <p>Every bot is expected to respond with a string representing their name (newline-terminated) within fifteen seconds.</p>

                <h3>Turn</h3>

                <p>Every turn, every bot is sent the the present game map (newline-terminated). Every bot is expected to respond with a set of moves (newline-terminated) within one second.</p>

                <h2>Networking Specifics</h2>

                <h3>Input Game Map Format</h3>

                <p>The state of the map (including owner and strength values, but excluding production values) is sent in the following way:
                <ul>
                    <li>One integer, <code>COUNTER</code>, representing the number of tiles with the same owner consecutively.</li>
                    <li>One integer, <code>OWNER</code>, representing the owner of the tiles <code>COUNTER</code> encodes.</li>
                </ul>
                <p>The above repeats until the <code>COUNTER</code> total is equal to the area of the map. It fills in the map from row 1 to row <code>HEIGHT</code> and within a row from column 1 to column <code>WIDTH</code>. Please be aware that the top row is the first row, as Halite uses screen-type coordinates.</p>
                <p>This is then followed by <code>WIDTH</code> * <code>HEIGHT</code> integers, representing the strength values of the tiles in the map. It fills in the map in the same way owner values fill in the map.</p>
                <p>Consider the following 3x3 map as an example (where <code>[O=x,S=y]</code> represents a tile owned by player x with strength y):<br><pre><code>[O=0,S=122] [O=1,S=25] [O=1,S=18]
[O=0, S=13] [O=0,S=45] [O=1,S=22]
[O=2,S=255] [O=2,S=85] [O=0, S=0]</code></pre>
                <p>This map would be encoded using the following string:
                <pre><code>1 0 2 1 2 0 1 1 2 2 1 0 122 25 18 13 45 22 255 85 0</code></pre>

                <h3>Input Production Format</h3>

                <p>The production values of the map are sent using <code>WIDTH</code> * <code>HEIGHT</code> integers which fill in the production values of the map from row 1 to row <code>HEIGHT</code> and within a row from column 1 to column <code>WIDTH</code>
Consider the following 3x3 production map as an example (where <code>[x]</code> represents a tile with x production):<br><pre><code>[2][3][4]
[1][2][3]
[0][1][2]</code></pre>
                <p>This map would be encoded using the following string:<br>
                <pre><code>2 3 4 1 2 3 0 1 2</code></pre>

                <h3>Output Move Set Format</h3>

                <p>Bots should send their moves as a list of integers in sets of 3. In each set, every first integer is the x location (starting at 0) of the site the bot desires to move, every second integer is the y location (starting at 0) of the site the bot desires to move, and every third integer is the direction the bot wishes to move the site in. The order of the sets does not matter.</p>
                
                <p>Valid directions include:
                <ul>
                    <li>0 - STILL</li>
                    <li>1 - NORTH</li>
                    <li>2 - EAST</li>
                    <li>3 - SOUTH</li>
                    <li>4 - WEST</li>
                </ul>
                <p>Please note that these directions correspond most directly to screen coordinates; that is, NORTH decrements the y value of the site by 1 and SOUTH increments the value by 1. Attempts to move nonexistent or enemy pieces or to move pieces in nonexistent directions will be ignored. If multiple separate moves are issued for the same piece, the lower direction value will be preferred.</p>
                <p>Consider the following case as an example:<br>I wish to order a piece located at (3, 4) to move <code>EAST</code>, a piece located at (4, 0) to remain <code>STILL</code>, and a piece located at (4, 5) to move <code>NORTH</code>.<br>This would be encoded with the following string:<br><pre><code>3 4 2 4 0 0 4 5 1</code></pre></p>

                <p>Note: if you would like an alternate way of communicating with the environment, please see a future post on the forums for how to communicate with the environment over sockets instead of pipes if you need to use a debugger or similar to work on your bot.</p>

                <h2>Submitting Your New Starter Package</h2>

            <p><a href="https://github.com/HaliteChallenge/Halite">Fork our repo</a>, place your starter package in the airesources/ folder, and send us a pull request! If we accept your PR, your starter package will be added to the site.<br>Note: please include the runGame.sh and runGame.bat scripts (each run a 30 by 30 game between MyBot and RandomBot) and the MyBot and RandomBot files (both just random bots).</p>

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
