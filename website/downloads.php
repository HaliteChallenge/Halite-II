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
				<h4>Changelog</h4>
				<p>
					This set of binaries was uploaded at <b>noon EST on July 12th</b>. Changes include:
					<ul>
						<li>Dynamic timeouts: instead of getting a second per turn, you will get fifty milliseconds plus half a millisecond per tile per turn. Example: on a 40 by 40 map, you will get 850 milliseconds per turn.</li>
						<li>A production graph in the visualizer</li>
						<li>Extensive visualizer logging: if you have problems with the visualizer, run it with the <code>-v</code> argument and send us the <code>debug.log</code> file that it outputs.</li>
					</ul>

				</p>
				<h4>Downloads</h4>
				<p>
						<ul>
						<li><a href="downloads/Halite-Mac.zip">Mac</a></li>
						<li><a href="downloads/Halite-Debian.zip">Ubuntu (64 bit)</a></li>
						<li><a href="downloads/Halite-Windows.zip">Windows</a></li>
					</ul>
				</p>

				<h3>Language Packages</h3>
				<h4>Changelog</h4>
				<p>This set of starter packages was uploaded at <b>noon EST on July 12th</b>. We standardized the starter packages naming scheme and changed getDistance to consider Manhattan distance.</p>

				<h4>Downloads</h4>
				<p>
					<ul>
						<li><a href="downloads/Halite-Python-Starter-Package.zip">Python 3</a></li>
						<li><a href="downloads/Halite-Java-Starter-Package.zip">Java 6+</a></li>
						<li><a href="downloads/Halite-C++-Starter-Package.zip">C++ 11</a></li>
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
