<!DOCTYPE html>
<html lang="en">

<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<meta name="google-site-verification" content="UzLAOvN92N2iaw_7HcFXSOc_M-WIe3KFXaozuaNsZo4" />
	<title>Halite Visualizer</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
</head>

<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div id="pageContent" class="pageContent">

			<div id="gameArea">
			</div>
			<div id="plots"></div>
		</div>
	</div>

	<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
	<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
	<script src="https://d3js.org/d3.v4.min.js"></script>
	<script src="script/backend.js"></script>
	<script src="script/general.js"></script>
	<script src="script/visualizer.js"></script>
</body>

</html>
