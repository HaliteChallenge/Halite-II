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
									<label for="login_user" class="control-label">Email</label>
									<input type="email" class="form-control" id="login_user">
									<input type="submit" style="display: none;">
								</div>

								<div class="form-group label-floating is-empty">
									<label for="login_pass" class="control-label">Password</label>
									<input id="login_pass" class="form-control" type="password" size="30" >
									<span class="material-input"></span>
								</div>
								<input id="loginButton" class="btn btn-primary" style="clear: left; width: 100%; height: 32px; font-size: 13px; margin-bottom:0px" type="submit" name="commit" value="Login" />
							</ul>
						</li>

						<li class="dropdown">
							<a class="dropdown-toggle" href="#" data-toggle="dropdown">Register<strong class="caret"></strong></a>
							<ul id="registerForm" class="dropdown-menu" style="padding: 15px; padding-bottom: 0px;">
								<div class="form-group label-floating is-empty">
									<label for="register_first" class="control-label">First Name</label>
									<input id="register_first" class="form-control" type="text" size="30" >
									<span class="material-input"></span>
								</div>
								<div class="form-group label-floating is-empty">
									<label for="register_last" class="control-label">Last Name</label>
									<input id="register_last" class="form-control" type="text" size="30" >
									<span class="material-input"></span>
								</div>
								<div class="form-group label-floating is-empty">
									<label for="register_email" class="control-label">Email</label>
									<input id="register_email" class="form-control" type="email" size="30" >
									<span class="help-block" id="schoolField">Enter your school email.</span>
									<span class="material-input"></span>
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
						<li><a href="#" id="submitButton">Submit To Current Competition</a><input type="file" id="myFile" name="outputFile" onchange="fileChanged()"></li>
						<li><a href="#" id="logoutButton">Logout</a></li>
					</ul>
				</form>
			</div>
		</div>
	</nav>
	
	<div id="messageBox"></div>

	<div class="container">
		<div class="pageContent">
			<div class="jumbotron" id="jumbotron">
				<h1 id="jHeader">Halite</h1>
				<p id="jParagraph">Filler filler filler</p>
			</div>
			<div class="row">
				<div class="col-sm-5">
					<div class="panel panel-primary">
						<div class="panel-heading">
							<h3 class="panel-title">Problem</h3>
						</div>
						<div class="panel-body" id="rulesPanelBody">
							Here is a description of halite.<br>
							<code>
								blah, blah<br>
								code, code
							</code>
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
									<th>Name</th>
									<th>Organization</th>
									<th>Score</th>
								</tr>
								<tbody>
									<tr>
										<th>1</th>
										<th>Michael Truell</th>
										<th>Google</th>
										<th>78</th>
									</tr>
									<tr>
										<th>2</th>
										<th>Ben Spector</th>
										<th>Yahoo</th>
										<th>2</th>
									</tr>
								</tbody>
							</thead>
							<tbody id="leaderboard">
							</tbody>
						</table>
					</div>
				</div>
			</div>
		</div>
	</div>

	<?php
		include 'includes/shaders.php';
	?>

	<script src="lib/jquery.min.js"></script>
	<script src="lib/bootstrap.min.js"></script>
	<script src="script/backend.js"></script>
	<script src="script/index.js"></script>
	<script src="script/visualizer.js"></script>	
</body>

</html>
