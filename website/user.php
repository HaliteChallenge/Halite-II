<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title></title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/index.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="jumbotron" id="jumbotron">
			<h1 id="jHeader"></h1>
			<p id="jBody"></p>
		</div>
		<div class="row">
			<div class="col-md-5">
				<div class="panel panel-primary">
					<div class="panel-heading">
						<h3 class="panel-title">Detailed Statistics</h3>
					</div>
					<table class="table well well-sm" id="statTable">
						<tbody id="statTableBody">
						</tbody>
					</table>
				</div>
			</div>
			<div class="col-md-7">
				<div class="panel panel-primary">
					<div class="panel-heading">
						<h3 class="panel-title">Past Games</h3>
					</div>
					<table class="table well well-sm" id="gameTable">
						<thead>
							<tr>
								<th>Opponent</th>
								<th>Result</th>
								<th>Gamefile</th>
							</tr>
						</thead>
						<tbody id="gameTableBody">
						</tbody>
					</table>
				</div>
			</div>
		</div>
	</div>

	<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
	<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
	<script src="script/backend.js"></script>
	<script src="script/general.js"></script>
	<script src="script/user.js"></script>
</body>
</html>
