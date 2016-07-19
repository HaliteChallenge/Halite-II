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
				<h1>Tools Specification</h1>

				<h3>Environment</h3>
				<p>
					The environment may be passed a number of flags, including:
					<ul>
						<li><code>-t</code>: disables timeouts for the duration of the game</li>
						<li><code>-q</code>: turns on quiet output. Output will take the form of:
							<ul>
								<li>A line containing replay file name</li>
								<li>For <code>n</code> players in the game, <code>n</code> lines like so: <code>rank playerID territoryValue strengthValue productionValue stillMovePercentage averageLatency</code></li>
								<li>A line of space separated playerIDs of the players that timed out</li>
							</ul>
						</li>
						<li><code>-s</code>: allows the overriding of names</li>
						<li><code>-w</code>: after the current game ends, the environment will start a <code>visualizer</code> binary with the current game if a <code>visualizer</code> binary exists in the current directory.</li>
					</ul>
					
					Flags must be prefixed by their own dash (<code>-</code>) and must be separated by other flags/arguements by a space. They may <b>not</b> be combined like so: <code>-qst</code>.
				</p>
				
				<h3>Visualizer</h3>
			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
					<li class="next"><a href="contest_spec.php">Contest Spec <span aria-hidden="true">&rarr;</span> </a></li>
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
