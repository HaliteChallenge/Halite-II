<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>Tutorial</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/index.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
			<div class="col-sm-12">
				<h1>Basic Tutorial</h1>

				<h3>Prerequisites</h3>
				<p></p>

				<h3>Run a Game</h3>
				<p>While working on your bot, you will want to run and visualize games locally. To do this, you will need the game environment, which runs games and outputs replay files, and the desktop visualizer, which visualizes the environment's replay files. These are located <a href="downloads.php">here</a>.</p>

				<p>Once those are downloaded, unzip you starter package. If you are using a compiled language, please compile the <code>MyBot</code> file of the starter package. We are going to run a game between three instances of the provided starter bot, which simply moves all of its peices randomly. To do this, run the environment binary. It should promt you for the game's dimensions (40 by 40 is perfectly resonable) and the command to start your bot. Examples of these commands include:
					<ul>
						<li>Java - <code>cd PATH_TO_BOT; java MyBot</code></li>
						<li>Python - <code>python3 PATH_TO_BOT/MyBot.py</code></li>
						<li>C++ - <code>PATH_TO_BOT/BOT_BINARY_NAME</code></li>
					</ul>
				The environment should then execute your game, outputting the turn number intermittently and finishing with a ranking of each of the instances of your bot.
				</p>

				<h3>Basic Bot</h3>
				<p></p>

				<h3>What's Next?</h3>
				<p></p>
			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
					<li class="previous"><a href="quickstart.php"><span aria-hidden="true">&larr;</span> Quickstart</a></li>
					<li class="next"><a href="contest_rules.php">Game Rules <span aria-hidden="true">&rarr;</span></a></li>
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
