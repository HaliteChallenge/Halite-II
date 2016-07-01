<nav class="navbar navbar-default" id="navvy">
	<div class="container-fluid">
		<div class="navbar-header">
			<button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
				<span class="sr-only">Toggle navigation</span>
				<span class="icon-bar"></span>
				<span class="icon-bar"></span>
				<span class="icon-bar"></span>
			</button>
			<a class="navbar-brand" href="index.php">Halite</a>
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
								<label for="register_email" class="control-label">Email</label>
								<input id="register_email" class="form-control" type="email" size="30" >
							</div>
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
					<li><a href="#" id="submitButton">Submit To Competition</a><input type="file" id="myFile" name="botFile"></li>
					<li><a href="#" id="logoutButton">Logout</a></li>
				</ul>
			</form>
		</div>
	</div>
</nav>
