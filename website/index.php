<!DOCTYPE html>
<html lang="en">

<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">

	<title>Halite</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/index.css" rel="stylesheet">
</head>

<body>
	<div class="container">
		<nav class="navbar navbar-default" id="navvy">
			<div class="container-fluid">
				<div class="navbar-header">
					<button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
						<span class="sr-only">Toggle navigation</span>
						<span class="icon-bar"></span>
						<span class="icon-bar"></span>
						<span class="icon-bar"></span>
					</button>
					<a class="navbar-brand" href="/">Halite</a>
				</div>
				<div id="navbar" class="navbar-collapse collapse">
					<ul class="nav navbar-nav navbar-right loggedOut" id="loginNav">
						<ul class="nav navbar-nav">
							<li class="dropdown">
								<a class="dropdown-toggle" href="#" data-toggle="dropdown">Login<strong class="caret"></strong></a>
								<ul id="loginForm" class="dropdown-menu" style="padding: 10px; padding-bottom: 0px;">
									<div class="form-group label-floating is-empty">
										<label for="login_user" class="control-label">Username</label>
										<input type="username" class="form-control" id="login_user">
										<input type="submit" style="display: none;">
									</div>

									<div class="form-group label-floating is-empty">
										<label for="login_pass" class="control-label">Password</label>
										<input id="login_pass" class="form-control" type="password" size="30" >
										<span class="material-input"></span>
									</div>
									<input id="loginButton" class="btn btn-primary" style="clear: left; width: 100%; height: 32px; font-size: 13px; margin-bottom:15px" type="submit" name="commit" value="Login" />
								</ul>
							</li>

							<li class="dropdown">
								<a class="dropdown-toggle" href="#" data-toggle="dropdown">Register<strong class="caret"></strong></a>
								<ul id="registerForm" class="dropdown-menu" style="padding: 15px; padding-bottom: 0px;">
									<div class="form-group label-floating is-empty">
										<label for="register_user" class="control-label">Username</label>
										<input id="register_user" class="form-control" type="username" size="30" >
									</div>
									<div class="form-group label-floating is-empty">
										<label for="register_pass" class="control-label">Password</label>
										<input id="register_pass" class="form-control" type="password" size="30" >
										<span class="material-input"></span>
									</div>
									<input id="registerButton" class="btn btn-primary" style="clear: left; width: 100%; height: 32px; font-size: 13px; margin-bottom:15px" type="submit" name="commit" value="Register" />
								</ul>
							</li>
						</ul>
					</ul>
					<form id="submitForm">
						<ul class="nav navbar-nav navbar-right loggedIn" id="logoutNav">
							<li><a href="#" id="submitButton">Submit To Current Competition</a><input type="file" id="myFile" name="botFile"></li>
							<li><a href="#" id="logoutButton">Logout</a></li>
						</ul>
					</form>
				</div>
			</div>
		</nav>
		
		<div id="messageBox"></div>
		<div class="pageContent">
			<div class="jumbotron" id="jumbotron">
				<h1 id="jHeader">Halite</h1>
				<p id="jParagraph">A multi-player turn-based strategy game</p>
			</div>
			<div class="row">
				<div class="col-sm-5">
					<div class="panel panel-primary">
						<div class="panel-heading">
							<h3 class="panel-title">Problem</h3>
						</div>
						<div class="panel-body" id="rulesPanelBody">
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
				<div class="col-sm-7">
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
	<script src="script/index.js"></script>
	<script src="script/visualizer.js"></script>	
</body>

</html>
