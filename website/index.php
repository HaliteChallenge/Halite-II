<!DOCTYPE html>
<html lang="en">

<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<meta name="google-site-verification" content="UzLAOvN92N2iaw_7HcFXSOc_M-WIe3KFXaozuaNsZo4" />
	<title>Halite</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/index.css" rel="stylesheet">
</head>

<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div id="messageBox"></div>
		<div class="pageContent">
			<div class="jumbotron" id="jumbotron">
				<h1 id="jHeader">Halite</h1>
				<p id="jParagraph">A multi-player turn-based strategy game</p>
				<p>
					<a href="quickstart.php" class="btn btn-info jumbotron-btn" role="button">Quickstart</a>
					<a href="tutorial.php" class="btn btn-info jumbotron-btn" role="button">Tutorial</a>
					<a href="contest_rules.php" class="btn btn-info jumbotron-btn" role="button">Game Rules</a>
					<a href="downloads.php" class="btn btn-primary jumbotron-btn" role="button">Downloads</a>
				</p>
			</div>
			<div class="row">
				<div class="col-sm-12">
					<div class="panel panel-primary">
						<div class="panel-heading">
							<h3 class="panel-title">Leaderboard</h3>
						</div>

						<table class="table well well-sm" id="leaderTable">
							<thead>
								<tr>
									<th>#</th>
									<th>Username</th>
									<th>Language</th>
									<th>Submissions</th>
									<th>Games Played</th>
									<th>Score</th>
								</tr>
							</thead>
						</table>
					</div>
				</div>
			</div>
		</div>
	</div>

	<?php
		include 'includes/shaders.php';
		include 'includes/game.php';
	?>

	<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
	<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
	<script src="script/backend.js"></script>
	<script src="script/general.js"></script>
	<script src="script/index.js"></script>
	<script src="script/visualizer.js"></script>
</body>

</html>
