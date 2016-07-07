<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>Game Rules</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
			<div class="col-sm-12">

				<h3>Overview</h3>

				<p>Halite is a multi-player turn-based strategy game played on a rectangular grid. The objective of the game is to take over the entire map, in competition with every other player in the game. Players use their territory to gain strength, and their strength to gain territory; they must move intelligently on both the micro and macro scales to play effectively.</p>

				<h3>Game Description</h3>

				<p>During a move, every piece you control can be given one of five moves: to move North, East, South, West, or to remain Still. When a piece remains where it is during a turn, it will permanently increase its Strength by the Production of the tile it sits on.</li>

				<p>Players gain pieces by simply moving their own pieces over Sites on the map. When a piece moves off of a Site, it leaves behind a piece with an identical Owner and with a Strength of 0, in doing so expanding the size of their territory.</p>

				<p>When pieces from the same player try to occupy the same site, the resultant piece has the sum of their strengths. Strengths are capped at 255, so if two pieces with a strength of 150 were to both attempt to occupy the same square, the resultant piece would still have a strength of 255. When pieces from opposing players try to occupy either the same or adjacent sites, the battle will be resolved according to the relative strengths of the pieces, as each piece decreases the Strength of every adjacent or coinciding opposing piece by its own Strength. Pieces with a strength of 0 or less are removed from the game, excepting pieces with a strength of 0 which have not engaged in combat during that turn.</p>

				<p>The map is initialized by the environment to have productions and strengths. Combat with map squares works identically to combat with other players except only applies on that square; empty map squares neither apply damage to nor take damage from adjacent squares. Players should note that the map does wrap around; if a piece at the top of the map moves North, it will reappear on the bottom of the map, and pieces on the top and bottom of the map will engage in combat (provided that they are in the same column).</p>

				<p>Players are scored according to the reverse of the order in which they are destroyed. The player last standing wins, whereas the player which was destroyed first comes in last. Bots are scored relatively; in a game with many players it is far better to come in second than to come in last.</p>

				<p>The game ends if one of two conditions are met:
					<ul>
				 		<li>Only one player has any pieces left.</li>
				 		<li>A certain number of turns has been reached. This number will vary depending on the size of the map in question; a small map may end after a few hundred turns, whereas a large map may take up many thousands.</li>
				 	</ul>
				</p>

				<p>The number of turns is generally given to be high enough that only the best-matched of bots will reach the turn limit; the vast majority of games will end before the turn limit is reached. In the event that the turn limit is reached or multiple bots are destroyed on the same turn, they are deem to have tied within the game.</p>

				<h3>Game Specification</h3>

				<h4>Initialization</h4>

				<p>At the start of the game, each bot is sent some information (accessed using getInit in the starter packages):
					<ul>
				 		<li>Their own tag within the map - that is, which player they are.</li>
				 		<li>The initial map state.</li>
				 	</ul>
				</p>

				<p>Bots are given three seconds at the start of the game to initialize. This initialization might include (but is in no way limited to) getting the initial map and player tag, identifying important, high-production regions on the map, identifying the locations of neighboring players, and/or planning the bot's initial expansion strategy. Once bots are done initializing (before their three seconds are up), they should send a response (sendInit in the starter packages) with their own player name, used for human identification purposes.</p>

				<h4>Turns</h4>

				<p>After all bots have finished setting up, the environment will do the following until endgame conditions are met.
					<ol>
				 		<li>Send the present gamestate - map and messages - to all players.</li>
				 		<li>Receive moves from the players.</li>
				 		<li>Add strength to pieces which choose to remain where they are.</li>
				 		<li>Simultaneously move all player's pieces.</li>
				 		<li>Simultaneously damage (and remove if damage exceeds strength) all player's pieces. All pieces will output damage equivalent to their strength when starting this phase, and the damage will apply to all coinciding or adjacent enemy squares.</li>
				 		<li>Check if endgame conditions have been met.</li>
				 	</ol>
				</p>

				<h4>Maps</h4>

				<p>Maps are randomly generated at the start of each game. The generator does the following:
					<ol>
				 		<li>Try to match the given width and height as closely as it can while still creating a symmetric map. Maps are guaranteed to be the given size or smaller in each dimension; never larger.</li>
				 		<li>Try to create interesting maps in which there are patches of high production squares and patches of low production squares, with fairly low noise on the small scale.</li>
				 		<li>Try to relatively match the production of tiles on the map with their starting strengths. That is, a square with a production of 3 will likely not have a strength of 3, but rather will usually have a higher strength than a tile with a production of 2 and a lower strength than a tile with a production of 4.</li>
				 		<li>Always creates symmetric maps. Specifically, the generator generates a chunk of the map and then tesselates it to produce the entire map.</li>
				 	</ol>
				</p>

				<h4>Files</h4>

				<p>Once a game ends, the environment will output it as a file for replaying at high speed later. Presently, the file does not contain the decisions of the players; only the game states resolved by the environment. A sample file should look like:<br>
					<pre><code>HLT 8 <-- A header to distinguish version and ensure that the file will be valid.
WIDTH HEIGHT NUM_PLAYERS NUM_FRAMES <-- The number of frames is always one more than the number of turns.
Name1 r1 g1 b1 <-- The player name and the color to map the player to.
Name2 r2 g2 b2
...
NameU rU gU bU
[WIDTH * HEIGHT bytes] <-- Represents the production of the map. Fills in the map by row, and within a row by column, where each byte is the production of that square.
FRAME(1)FRAME(2)FRAME(3)...FRAME(NUM_FRAMES)</code></pre>
					An individual FRAME(#) consists of the following format:
					<pre><code>One byte, COUNTER, representing the number of tiles with the same owner consecutively.
One byte, OWNER, which is the owner the preceding COUNTER relies on.
COUNTER bytes, representing the individual strengths of the pieces encoded.
The above is repeated until the sum of all COUNTERs is equal to WIDTH * HEIGHT. As with the productions, the frame fills in the map by row, and within a row by column.</code></pre>
				</p>

			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
					<li class="previous"><a href="tutorial.php"><span aria-hidden="true">&larr;</span> Tutorial</a></li>
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
