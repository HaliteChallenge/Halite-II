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
				<p><b>Download the starter package <a href="../StarterPackage.zip">here</a></b></p>

				<h3>Overview</h3>

				<p>Halite is a multi-player turn-based strategy game played on a rectangular grid. The objective of the game is to take over the entire map, in competition with every other player in the game. Players use their territory to gain strength, and their strength to gain territory; they must move intelligently on both the micro and macro scales to play effectively.</p>

				<h3>Game Description</h3>

				<p>During a move, every piece you control can be given one of five moves: to move North, East, South, West, or to remain Still. When a piece remains where it is during a turn, it will permanently increase its Strength by the Production of the tile it sits on.</li>

				<p>Players gain pieces by simply moving their own pieces over Sites on the map. When a piece moves off of a Site, it leaves behind a piece with an identical Owner and with a Strength of 0, in doing so expanding the size of their territory.</p>

				<p>When pieces from the same player try to occupy the same site, the resultant piece has the sum of their strengths. Strengths are capped at 255, so if two pieces with a strength of 150 were to both attempt to occupy the same square, the resultant piece would still have a strength of 255. When pieces from opposing players try to occupy either the same or adjacent sites, the battle will be resolved according to the relative strengths of the pieces, as each piece decreases the Strength of every adjacent or coinciding opposing piece by its own Strength. Pieces with a strength of 0 or less are removed from the game, excepting pieces with a strength of 0 which have not engaged in combat during that turn.</p>

				<p>The map is initialized by the environment to have productions and strengths. Combat with map squares works identically to combat with other players except only applies on that square; empty map squares neither apply damage to nor take damage from adjacent squares. Players should note that the map does wrap around; if a piece at the top of the map moves North, it will reappear on the bottom of the map, and pieces on the top and bottom of the map will engage in combat (provided that they are in the same column).</p>

				<p>Players are scored according to the reverse of the order in which they are destroyed. The player last standing wins, whereas the player which was destroyed first comes in last. Bots are scored relatively; in a game with many players it is far better to come in second than to come in last.</p>

				<p>The game ends if one of two conditions are met:
					<ul>
				 		<li>Only one player has any pieces left.</li>
				 		<li>A certain number of turns has been reached. This number will vary depending on the size of the map in question; a small map may end after a few hundred turns, whereas a large map may take up many thousands.</li>
				 	</ul>
				</p>

				<p>The number of turns is generally given to be high enough that only the best-matched of bots will reach the turn limit; the vast majority of games will end before the turn limit is reached. In the event that the turn limit is reached or multiple bots are destroyed on the same turn, they are deem to have tied within the game.</p>
			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
					<li class="previous"><a href="quickstart.php"><span aria-hidden="true">&larr;</span> Quickstart</a></li>
					<li class="next"><a href="contest_rules.php">Contest Rules <span aria-hidden="true">&rarr;</span></a></li>
				</ul>
			</div>
		</div>
	</footer>
</body>
</html>
