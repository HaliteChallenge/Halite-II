<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>File Spec</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
			<div class="col-sm-12">
				<h1>File Specification</h1>
				<p>A sample file should look like:<br>
						<pre><code>HLT 9 <-- A header to distinguish version and ensure that the file will be valid. This is followed by a newline.
WIDTH HEIGHT NUM_PLAYERS NUM_FRAMES <-- The number of frames is always one more than the number of turns. Note that NUM_FRAMES is always 1 more than NUM_TURNS. This is followed by a newline.
Name1'\0'r1 g1 b1 <-- The player name and the color to map the player to, separated by a null character. Each of these lines is followed by a newline.
Name2'\0'r2 g2 b2
...
NameU'\0'rU gU bU
[WIDTH * HEIGHT bytes] <-- Represents the production of the map. Fills in the map by row, and within a row by column, where each byte is the production of that square. This is followed by a newline.
FRAME(1)MOVES(1)FRAME(2)MOVES(2)FRAME(3)...MOVES(NUM_TURNS)FRAME(NUM_FRAMES)</code></pre>
				</p>
				<p>
						An individual FRAME(#) consists of the following format:
						<pre><code>One byte, COUNTER, representing the number of tiles with the same owner consecutively.
One byte, OWNER, which is the owner the preceding COUNTER relies on.
COUNTER bytes, representing the individual strengths of the pieces encoded.
The above is repeated until the sum of all COUNTERs is equal to WIDTH * HEIGHT. As with the productions, the frame fills in the map by row, and within a row by column.</code></pre>			
				</p>
				<p>
					An individual MOVE(#) consists of the following format:
					<pre><code>WIDTH * HEIGHT bytes, corresponding to the ordered (and therefore executed) moves of every square on the map for the preceding game state.
The values correspond with directions as they do when bots interact with the environment; namely, 0 = STILL, 1 = NORTH, 2 = EAST, 3 = SOUTH, and 4 = WEST.
As with the productions, the frame fills in the map by row, and within a row by column. Squares owned by the null player are assumed to have moved STILL.</code></pre>
				</p>

			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
					<li class="previous"><a href="game_spec.php"><span aria-hidden="true">&larr;</span> Game Spec</a></li>
					<li class="next"><a href="networking_spec.php">Networking Spec <span aria-hidden="true">&rarr;</span> </a></li>
				</ul>
			</div>
		</div>
	</footer>

	<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
	<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
	<script src="script/backend.js"></script>
	<script src="script/general.js"></script>
</body>
</html>
