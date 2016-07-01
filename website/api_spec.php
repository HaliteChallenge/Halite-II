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

				<h3>Overview</h3>

				<p>To make the interaction of bots with the Halite environment as simple as possible, bots communicate via stdin and stdout, sending strings of space-separated integers ended with a newline.</p>

				<p>There are two separate formats of information that are sent between the environment and bots.
					<ul>
				 		<li>Initialization</li>
				 		<li>Frame</li>
				 	</ul>
				 Bots are initialized only once at the start of the game, before the first frame. Following initialization, bots follow the frame format until the end of the game, when the environment will automatically terminate them.</p>


				<h3>Initialization</h3>

				<p>Bots are sent the following, each on their own line:
					<ul>
				 		<li>A single integer representing their own tag within the game.</li>
				 		<li>Two integers respresenting the WIDTH and HEIGHT of the map.</li>
				 		<li>WIDTH * HEIGHT integers respresenting the production values of the map. The integers fill in the map by row, and within a row by column.</li>
				 		<li>The initial game map (in the format discussed below)</li>
				 	</ul>
				 </p>
				 <p>Bots are expected to respond with a single string containing their name.</p>


				<h3>Frame</h3>

				<p>Bots are sent the following, each on their own line:
					<ul>
				 		<li>A single integer representing their own tag within the game.</li>
				 		<li>Two integers respresenting the WIDTH and HEIGHT of the map.</li>
				 		<li>WIDTH * HEIGHT integers respresenting the production values of the map. The integers fill in the map by row, and within a row by column.</li>
				 		<li>The initial game map (in the format discussed below)</li>
				 	</ul>
				 </p>
				 <p>Bots are expected to respond with a single string containing their name.</p>

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
