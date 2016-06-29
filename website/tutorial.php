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

				<p>During a move, every piece you control can be given one of five moves: to move North, East, South, West, or to remain Still. When a piece remains where it is during a turn, two things will happen to it:
					<ul>
				 		<li>It will permanently increase its Strength by one.</li>
						<li>It will receive, for the duration of that turn, a temporary strength boost.</li>
					</ul>
				</p>

				<p>Players gain pieces by simply moving their own pieces over Sites on the map. When a piece moves off of a Site, it leaves behind a piece with an identical Owner and with a Strength of 0, in doing so expanding the size of their territory.</p>

				<p>When pieces from the same player try to occupy the same site, the resultant piece has the sum of their strengths. Real strengths are capped at 255 (although strength boosts can push strengths above that). When pieces from opposing players try to occupy either the same or adjacent sites, the battle will be resolved according to the relative strengths of the pieces, as each piece decreases the Strength of every adjacent or coinciding opposing piece by its own Strength. Pieces with a strength of 0 or less are removed from the game, excepting those which have not been in combat during that turn.</p>

				<p>Players should note that the map does wrap around; if a piece at the top of the map moves North, it will reappear on the bottom of the map, and opposing pieces on the top and bottom of the map will engage in combat (provided that they are in the same column).</p>

				<p>Throughout the game, scores are calculated as a sum of all territory that each player has controlled throughout the game. Players which survive until the end of the game receive a 2x bonus to their score. Scores are calculated purely for ranking players in games (i.e. First, Second, Third place...).</p>

				<p>The game ends if one of two conditions are met:
					<ul>
				 		<li>Only one player is left.</li>
				 		<li>A certain number of terms has been reached. This number will vary depending on the size of the map in question; a small map may end after a hundred moves or so, whereas a large map may take up to thousands.</li>
				 	</ul>
				</p>
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
