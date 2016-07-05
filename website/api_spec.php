<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>API Spec</title>

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
				 		<li>Two integers respresenting the <code>WIDTH</code> and <code>HEIGHT</code> of the map.</li>
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
				 		<li>The present game map.</li>
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

				<h4>Input Game Map Format:</h4>

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

				<h4>Input Production Format:</h4>

				<p>The production values of the map are sent using <code>WIDTH</code> * <code>HEIGHT</code> integers which fill in the production values of the map from row 1 to row <code>HEIGHT</code> and within a row from column 1 to column <code>WIDTH</code></p>

				<p>Consider the following production map as an example:<br>[2] [3] [4]<br>[1] [2] [3]<br>[0] [1] [2]<br>This map would be encoded using the following string:<br>2 3 4 1 2 3 0 1 2</p>

				<h4>Input Alliance State Format:</h4>

				<p>The present state of alliances will be sent using ((<code>NUM_PLAYERS</code>)^2) integers. If one were to interpret the list of integers into a square grid, again filling spots from row 1 to row <code>NUM_PLAYERS</code> and within a row from column 1 to column <code>NUM_PLAYERS</code>, the grid would serve as a table for looking up the remaining numbers of turns that the two players are bound in alliance. Additionally, this means that the grid is symmetric over the line y=x, and furthermore grid spots along y=x will always be 0.</p>

				<p>Consider the following case as an example:<br>There are 4 players in the game. Players 1 and 4 are allied for 8 more turns, while players 2 and 3 are only bound in alliance for 3 more turns. We could then represent this setup with the following square grid:<br>0 0 0 8<br>0 0 3 0<br>0 3 0 0<br>8 0 0 0<br>This would in turn be encoded with the following string:<br>0 0 0 8 0 0 3 0 0 3 0 0 8 0 0 0</p>

				<h4>Input Alliance Request Set Format:</h4>

				<p>Bots are sent alliance requests as a list of integers in sets of 2. In each set, the first integer is the tag of the player which requested the alliance, and the second integer is the number of turns for which the alliance would be binding, starting with this (upcoming) turn. Bots will not receive more than one alliance request from any individual player, nor will they ever receive requests from themselves.</p>

				<p>Consider the following case as an example:<br>Player 3 wishes to enter an alliance for 20 moves, and player 6 wishes to enter an alliance for 5 moves.<br>This would be encoded with the following string:<br>3 20 6 5</p>

				<h4>Output Move Set Format:</h4>

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

				<h4>Output Alliance Request Set Format:</h4>

				<p>Bots should send alliance requests as a list of integers in sets of 2. In each set, the first integer is the tag of the player with whom the bot requests the alliance, and the second integer is the number of turns for which the alliance would be binding, starting with the next (not upcoming) turn. Bots should not send more than one alliance request to any other bot each turn.</p>

				<p>Consider the following case as an example:<br>A bot wishes to propose an alliance with player 2 for 12 moves and with player 1 for 120 moves.<br>This would be encoded with the following string:<br>2 12 1 120</p>


				<h4>Output Alliance Response Set Format:</h4>

				<p>Bots should send alliance requests as a list of integers in sets of 2. In each set, the first integer is the tag of the player which requested the alliance, and the second integer is either a 0 or 1, correspondingly representing a <code>DECLINE</code> or <code>ACCEPT</code> of the alliance, starting with this (upcoming) turn. Bots should not send more than one alliance response to any other bot each turn.</p>

				<p>Consider the following case as an example:<br>A bot would like to accept a proposed alliance with player 2 and decline a proposed alliance with player 5.<br>This would be encoded with the following string:<br>2 1 5 0</p>
			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
					<li class="previous"><a href="contest_rules.php"><span aria-hidden="true">&larr;</span> Game Rules</a></li>
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
