<!DOCTYPE html>
<html lang="en">

<head>
	<?php include 'includes/header.php'; ?>

	<title>Leaderboard</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
</head>

<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="pageContent">
			<div class="row">
				<div class="col-sm-12">
					<h1 id="leaderHeading">Leaderboard</h1>
					<p>These rankings are continuously updated. Want to get on the leaderboard? Visit our <a href="quickstart.php">quickstart guide</a>.</p>
					<div class="panel panel-primary">
						<?php include 'includes/leaderTable.php'; ?>
					</div>
				</div>
			</div>
		</div>
	</div>

	<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
	<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
	<script src="script/backend.js"></script>
	<script src="script/general.js"></script>
	<script src="script/leaderTable.js"></script>
	<script src="script/leaderboard.js"></script>
</body>

</html>
