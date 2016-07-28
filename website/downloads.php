<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>Downloads</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
			<div class="col-sm-12">
				<h1>Halite Downloads</h1>

				<h3>Environment and Visualizer</h3>
				<p>
					This set of binaries was uploaded at <b>11:20am EST on July 28th</b>. We changed the timeout format to be a little quicker and fairer. The number of milliseconds bots are allowed is given by the formula <code>15000 + WIDTH * HEIGHT * MAX_NUM_TURNS / 3</code>. We also added error logging - if a bot times out, it will output an error log for the game with the player tag of the bot and id of the game in the filename (use -q to get the filename for certain from the environment). There are also a bunch of small bugfixes for the environment.
				<p>
					<ul>
						<li><a href="downloads/Halite-Mac.zip">Mac</a></li>
						<li><a href="downloads/Halite-Debian.zip">Ubuntu (64 bit)</a></li>
						<li><a href="downloads/Halite-Windows.zip">Windows</a></li>
					</ul>
				</p>

				<h3>Language Packages</h3>
				<p>
					This set of binaries was uploaded at <b>1:10pm EST on July 16th</b>. We added a Rust starter package!
				</p>
				<p>
					<ul>
						<li><a href="downloads/Halite-Python-Starter-Package.zip">Python 3</a></li>
						<li><a href="downloads/Halite-Java-Starter-Package.zip">Java 7</a></li>
						<li><a href="downloads/Halite-C++-Starter-Package.zip">C++ 11</a></li>
						<li><a href="downloads/Halite-Rust-Starter-Package.zip">Rust 1.10</a></li>
					</ul>
				</p>
			</div>
		</div>
	</div>

	<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
	<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
	<script src="script/backend.js"></script>
	<script src="script/general.js"></script>
</body>
</html>
