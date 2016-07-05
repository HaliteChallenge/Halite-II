<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>Quickstart</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/index.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
			<div class="col-sm-12">
				<h1>Quickstart</h1>

				<h3>Register</h3>
				<p>Sign up for Halite with a Two Sigma email. Click on the link in the verification email that we sent you. Now log in to your account. You are ready to start competing!</p>

				<h3>Download a Starter Package</h3>
				<p>Halite is language agnostic; bots talk to the game environment using pipes. However, this requires some boilerplate code. We provide that boilerplate and a couple of basic Halite bots in the form of language "starter packages." Navigate <a href="downloads.php">here</a> to download the starter package for your preferred language. If we don't currently support your preferred language, you can send us an email at <a href="mailto:halite@halite.io">halite@halite.io</a>. You may also write your own starter package. The networking protocol that the game environment uses to interface with the starter packages is detailed <a href="api_spec.php">here</a>.</p>

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

				<p>To visualize the game that you just ran, run the visualizer binary and drop the game's replay file, ending with <code>hlt</code> and located in the directory of you environment binary.</p>

				<h3>Submit the Starter Bot</h3>
				<p>Users may submit their bots to Halite's website in order to qualify for the playoffs, to get their ranking, and to see their bot in match against others. In order to submit the random bot given in the starter packages, click the "Submit to Competition" button and upload a zip of the files inside your starter package's directory. Your bot should appear on the leaderboard within a couple of minutes.</p>

				<h3>Need Help? Have Feedback?</h3>
				<p>Don't hesitate to send us an email at <a href="mailto:halite@halite.io">halite@halite.io</a> or find either Michael Truell or Ben Spector on the 16th floor of 100 Avenue of the Americas. We work right next to the Hacker Lab.</p>
			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
					<li class="next"><a href="tutorial.php">Tutorial <span aria-hidden="true">&rarr;</span></a></li>
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
