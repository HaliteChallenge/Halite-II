<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>SP Spec</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/index.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
			<div class="col-sm-12">

				<h3>Starter Package Overview:</h3>

				<p>To make the interaction of bots with the Halite environment as simple as possible, bots communicate via stdin and stdout, sending strings of space-separated integers ended with a newline.<br>Although the api is fairly straightforward, to aid in the creation of bots we provide boilerplate code for some common languages (presently C++, Java, and Python 3).<br>Due to language differences, the syntax is not identical in each of these languages, but the overall function names and meanings are the same.</p>

				<h4>Initialization Input - getInit</h4>

				<p>This function reads in the initial game state for the bot. Internally, it also receives and decodes the production map and game size and stores a local copy, but it only returns the player tag and game map (which itself contains the production map).</p>			

				<h4>Initialization Output - sendInit</h4>

				<p>This function sends the desired player name back to the game environment. In sending the name, it effectively signals the environment that the player is ready for the start of the game, and the player should be prepared to immediately receive the first frame of the game (which will incidentally contain the same map as was received in getInit). sendInit is a separate function from getInit to give players time to initialize potential strategies after receiving the initial map, and should at latest be called three seconds after calling getInit, although probably earlier to provide a networking buffer.</p>

				<h4>Frame Input - getFrame</h4>

				<p>This function receives and decodes the present game map for the player from the environment. It will also reset the state of the productions and map size, so if your bot mangles the game map in its algorithm, fear not! - no changes will persist.</p>

				<h4>Frame Output - sendFrame</h4>

				<p>This function encodes and sends the moves from the player. This function will send all of the moves it is given, so it is recommended that the player carefully compile their list of moves before sending them with this function. This function should at latest be called one seconds after calling getFrame, although probably earlier to provide a networking buffer.</p>

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
