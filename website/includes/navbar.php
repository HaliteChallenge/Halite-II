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
					<?php include 'includes/dropdowns.php'; ?>
					<li>
					  <a href="https://github.com/login/oauth/authorize?scope=user:email&client_id=2b713362b2f331e1dde3">Login with Github</a>
					</li>
				</ul>
			</ul>
			<form id="submitForm">
				<ul class="nav navbar-nav navbar-right loggedIn" id="logoutNav">
					<?php include 'includes/dropdowns.php'; ?>
					<li><a href="#" id="submitButton">Submit</a><input type="file" id="myFile" name="botFile"></li>
					<li><a href="#" id="logoutButton">Logout</a></li>
				</ul>
			</form>
		</div>
	</div>
</nav>
<div id="messageBox"></div>
