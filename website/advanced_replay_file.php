<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Replay Files</title>

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
                <h1>Replay Files</h1>

                <h2>Overview</h2>

                <p>A Halite replay file is a JSON object which describes what occurred during the game. Halite replay files are easy to parse and reasonably compact (and extremely compact when compressed), making them ideal for both using machine learning on Halite as well as sharing interesting games. The extension .hlt is used to distinguish Halite replay files.</p>

                <h2>Detailed Specification</h2>

                <p>A Halite replay file contains only a single JSON object. This object has the following attributes:
                <ul>
                    <li><code>version</code> - Describes the version of the replay file. The present version is 11, which is expected to be stable for a while.</li>
                    <li><code>width</code> - A positive integer representing the width of the map.</li>
                    <li><code>height</code> - A positive integer representing the height of the map.</li>
                    <li><code>num_players</code> - A positive integer of at least 2 representing the number of players taking part in the game.</li>
                    <li><code>num_frames</code> - The number of frames encoded in this replay.</li>
                    <li><code>player_names</code> - An array of strings which correspond to the names of the players, starting with the player with tag 1 and ending with the player with tag <code>num_players</code>.</li>
                    <li><code>productions</code> - A 2D array of integers which represent the productions of the tiles on the map. These integers fill in the map from row 1 (i.e. the top) to row <code>height</code> and within a row from column 1 to column <code>width</code> and are all guaranteed to be positive and less than 255.
                    <li><code>frames</code> - A 4D arrays of integers. This is the most complex piece of the file specification, so here it is step by step:
                    <ul>
                        <li>The outermost array has length <code>num_frames</code>, and it contains individual frames.</li>
                        <li>The next array inwards has length <code>height</code>, and it contains individual rows of the frame. The first element is the first (top) row and the last is the bottom row.</li>
                        <li>The following array has length <code>width</code>, and it contains individual sites of the frame. The first element is the first (leftmost) column and the last is the rightmost row.</li>
                        <li>The innermost array has length 2 and contains integers. Its contents represent the attributes of a given site. The first element is always the owner of the site and the second element is the strength of the site.</li>
                    </ul></li>
                    <li><code>moves</code> - A 3D arrays of integers. The outermost array has length <code>num_frames - 1</code> and contains the moves for an individual frame. Each element is a 2D array of integers which represent the directions chosen to move for every site on the map. As always, the outer layer of the 2D array fills in the rows from top to bottom and the inside layer fills in individual rows from left to right. The integer values for each site are: <code>STILL</code> = 0, <code>NORTH</code> = 1, <code>EAST</code> = 2, <code>SOUTH</code> = 3, and <code>WEST</code> = 4. Sites not owned by any player are assigned the value of 0.</li>
                </ul></p>

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
