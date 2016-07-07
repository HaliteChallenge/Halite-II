<!DOCTYPE html>
<html lang="en">

<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<meta name="google-site-verification" content="UzLAOvN92N2iaw_7HcFXSOc_M-WIe3KFXaozuaNsZo4" />
	<title>Leaderboard</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
	<link href="style/discourse_login.css" rel="stylesheet">
</head>

<body>
	<div class="container">
		<div id="messageBox"></div>
		<div class="pageContent">
			<div class="row">
				<div class="col-sm-12">
					<div class="panel panel-primary">
						<div class="panel-heading">
							<h3 class="panel-title">Login</h3>
						</div>
						<form>
							<div class="form-group">
								<label for="login_user" class="control-label">Username</label>
								<input type="username" class="form-control" id="login_user">
							</div>
							<div class="form-group">
								<label for="login_pass" class="control-label">Password</label>
								<input id="login_pass" class="form-control" type="password" size="30" >
							</div>
							<div>
								<input id="loginButton" class="btn btn-primary" type="submit" name="commit" value="Login" />
							</div>
						</form>
					</div>
					<div class="panel panel-primary">
						<div class="panel-heading">
							<h3 class="panel-title">Register</h3>
						</div>
						<form>
							<div class="form-group">
								<label for="login_user" class="control-label">Username</label>
								<input type="username" class="form-control" id="login_user">
							</div>
							<div class="form-group">
								<label for="login_pass" class="control-label">Password</label>
								<input id="login_pass" class="form-control" type="password" size="30" >
							</div>
							<div class="form-group">
								<label for="login_pass" class="control-label">Confirm Password</label>
								<input id="login_pass" class="form-control" type="password" size="30" >
							</div>
							<div>
								<input id="loginButton" class="btn btn-primary" type="submit" name="commit" value="Login" />
							</div>
						</form>
					</div>
				</div>
			</div>
		</div>
	</div>

	<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
	<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
	<script src="script/backend.js"></script>
	<script src="script/index.js"></script>
	<script src="script/visualizer.js"></script>
</body>

</html>
