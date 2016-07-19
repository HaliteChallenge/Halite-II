<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>Tools Spec</title>

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
						<li><code>-d</code>: allows the automatic input of the dimensions of the map. The following two arguments are expected to be width and height./li>
						<li><code>-q</code>: turns on quiet output. Output will take the form of:
							<ul>
								<li>A line containing the replay file name, a space, and the map seed.</li>
								<li>For <code>n</code> players in the game, <code>n</code> lines like so: <code>rank playerID territoryValue strengthValue productionValue stillMovePercentage averageLatency</code></li>
								<li>A line of space separated playerIDs of the players that timed out.</li>
							</ul>
						</li>
						<li><code>-o</code>: allows the overriding of names. Once all of the flags and their arguments have been removed from the list of arguments, this will assume that every second argument is an override name. For example, <code>"python3 MyBot.py" -q -o -d 30 30 "This is a python bot!" "cd somedirectory; java MyBot" -w "This is a java bot!"</code> is a perfectly valid set of arguments.</li>
						<li><code>-s</code>: provides the seed to the map generator. If this is not provided, it will use a time-based seed.</li>
						<li><code>-w</code>: after the current game ends, the environment will start a <code>visualizer</code> binary with the current game if a <code>visualizer</code> binary exists in the current directory.</li>
						<li><code>-t</code>: disables timeouts for the duration of the game.</li>
					</ul>
					
					Flags must be prefixed by their own dash (<code>-</code>) and must be separated by other flags/arguements by a space. They may <b>not</b> be combined like so: <code>-qst</code>.
				</p>
				
				<h3>Visualizer</h3>
				<p>
					The visualizer may be passed either one or two arguments:
					<ul>
						<li><code>-v</code>: (for now, must be first arg) The visualizer will write incredibly verbose output to its debug.log file. This is very useful for when things break or strange bugs occur, but can significantly slow down the visualizer due to the continual flushing of the output stream.</li>
						<li><code>path/to/file.hlt</code>: Will automatically load in the file and start viewing it. Useful for when you don't want to drag a file onto screen.</li>
					</ul>
					Flags must be prefixed by their own dash (<code>-</code>) and must be separated by other flags/arguements by a space. They may <b>not</b> be combined like so: <code>-qst</code>.
				</p>

				<p>
					For help on the visualizer commands, we recommend pulling up the visualizer and pressing 'h' to show the help screen. In the event that something does not work (e.g. text rendering), the same commands are listed below:
					<ul>
						<li>To pause or unpause the replay, press SPACE</li>
						<li>To move around in the replay, press &#8592 or &#8594
						<ul>
							<li>Hold shift to move around five times faster.</li>
							<li>To change move and play speed, press &#8593 or &#8595
						</ul></li>
						<li>To move around in the replay by frame, press , or .</li>
						<li>To go to the beginning or end of the replay, press z or x</li>
						<li>To pan around in the map, use the w, a, s, and d keys.</li>
						<li>To zoom in or out on the graphs, press + or -</li>
						<li>To view the production map, hold TAB</li>
						<li>To reload a replay from file, press r</li>
						<li>To toggle fullscreen mode, press f</li>
						<li>To view this help panel, hold h</li>
					</ul>
				</p>
			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
					<li class="previous"><a href="game_spec.php"><span aria-hidden="true">&larr;</span> Game Spec</a></li>
					<li class="next"><a href="file_spec.php">File Spec <span aria-hidden="true">&rarr;</span> </a></li>
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
