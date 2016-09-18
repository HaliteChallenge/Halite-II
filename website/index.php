<?php if(isset($_SESSION['userID'])) header("Location: user.php");?>

<!DOCTYPE html>
<html lang="en">

<head>
	<?php include 'includes/header.php'; ?>

	<title>Leaderboard</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
	<style>
		canvas {
			border: 1px solid #000;
			border-radius: 5px;
			padding: 5px;
		}
	</style>
</head>

<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="pageContent">
			<div class="row">
				<div class="col-sm-12">
					<h1>Introducing Halite</h1>
					<p>Halite is a multiplayer turn-based game that is played by computer programs. (Insert longer description here).</p>
					<p>Interested? Visit our <a href="quickstart.php">quickstart tutorial</a>. The simplest Halite bot is just 14 lines of code.</p>
					<p>Computer programs duking it out in Halite:</p>
					<div id="gameArea"></div>
					<h1>Top Rankings</h1>
					<p>Here are some of the best bots playing Halite right now. Click on their usernames to see some of their recent games. Watching them might just give you some stratagy ideas.</p>
					<div class="panel panel-primary">
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

	<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
	<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
	<script src="script/backend.js"></script>
	<script src="script/general.js"></script>
	<script src="lib/pixi.min.js"></script>
	<script src="script/parsereplay.js"></script>
	<script src="script/visualizer.js"></script>
	<script src="script/index.js"></script>
</body>

</html>
