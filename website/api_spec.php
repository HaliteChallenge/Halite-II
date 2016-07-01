<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>Halite Rules</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/index.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
			<div class="col-sm-12">

				<h3>Overview:</h3>

				<p>To make the interaction of bots with the Halite environment as simple as possible, bots communicate via stdin and stdout, sending strings of space-separated integers ended with a newline. Bots are expected to communicate using the format found below; any deviation from it will likely result in the bot being ejected from the game.</p>

				<p>There are two separate formats of information that are sent between the environment and bots.
					<ul>
				 		<li>Initialization</li>
				 		<li>Frame</li>
				 	</ul>
				 </p>
				 
				 <p>Bots are initialized only once at the start of the game, before the first frame. Following initialization, bots follow the frame format until the end of the game, when the environment will automatically terminate them.</p>


				<h3>Initialization Format:</h3>

				<p>Every bot is sent the following, with each item newline-terminated:
					<ul>
				 		<li>A single integer representing their own tag within the game.</li>
				 		<li>Two integers respresenting the WIDTH and HEIGHT of the map.</li>
				 		<li>The production map.</li>
				 		<li>The initial game map.</li>
				 	</ul>
				</p>

				 <p>Every bot is expected to respond with the following, with each item newline-terminated:
				 	<ul>
				 		<li>A string representing their name.</li>
				 		<li>A set of alliance requests.</li>
				 	</ul>
				</p>

				<h3>Frame Format:</h3>

				<p>Every bot is sent the following, with each item newline-terminated:
					<ul>
				 		<li>The present game map (in the format described below).</li>
				 		<li>The present state of alliances.</li>
				 		<li>A set of alliance requests.</li>
				 	</ul>
				</p>

				<p>Every bot is expected to respond with the following, with each item newline-terminated:
				 	<ul>
				 		<li>A set of moves.</li>
				 		<li>A set of alliance requests.</li>
				 		<li>A set of alliance request responses.</li>
				 	</ul>
				</p>


				<h3>Format Specifics:</h3>

				<h4>Map Format:</h4>

				<p>The state of the map (including owner and strength values, but excluding production values) is sent in the following way:
				 	<ul>
				 		<li>One integer, COUNTER, representing the number of tiles with the same owner consecutively.</li>
				 		<li>One integer, OWNER, representing the owner of the tiles COUNTER encodes.</li>
				 	</ul>
				The above repeats until the COUNTER total is equal to the area of the map. It fills in the map from row 1 to row HEIGHT and within a row from column 1 to column WIDTH.<br>
				This is then followed by WIDTH * HEIGHT integers, representing the strength values of the tiles in the map. It fills in the map in the same way owner values fill in the map.
				</p>

				<p>Consider the following map as an example:<br>[O=0,S=122] [O=1,S=25] [O=1,S=18]<br>[O=0,S=13] [O=0,S=45] [O=1,S=182]<br>[O=2,S=255] [O=2,S=85] [O=0,S=0]<br>This map would be encoded using the following string:<br>1 0 2 1 2 0 1 1 2 2 1 0 122 25 18 13 45 182 255 85 0</p>

				<h4>Production Format:</h4>

				<p>The production values of the map are sent using WIDTH * HEIGHT integers which fill in the production values of the map from row 1 to row HEIGHT and within a row from column 1 to column WIDTH</p>

				<p>Consider the following production map as an example:<br>[2] [3] [4]<br>[1] [2] [3]<br>[0] [1] [2]<br>This map would be encoded using the following string:<br>2 3 4 1 2 3 0 1 2</p>

				<h4>Alliance State Format:</h4>

				<p>The present state of alliances will be sent using ((NUM_PLAYERS)^2) integers. If one were to interpret the list of integers into a square grid, again filling spots from row 1 to row NUM_PLAYERS and within a row from column 1 to column NUM_PLAYERS, the grid would serve as a table for looking up the remaining numbers of turns that the two players are bound in alliance. Additionally, this means that the grid is symmetric over the line y=x, and furthermore grid spots along y=x will always be 0.</p>

				<p>Consider the following case as an example:<br>There are 4 players in the game. Players 1 and 4 are allied for 8 more turns, while players 2 and 3 are only bound in alliance for 3 more turns. We could then represent this setup with the following square grid:<br>0 0 0 8<br>0 0 3 0<br>0 3 0 0<br>8 0 0 0<br>This would in turn be encoded with the following string:<br>0 0 0 8 0 0 3 0 0 3 0 0 8 0 0 0</p>

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
