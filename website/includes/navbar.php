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
						<a class="dropdown-toggle" href="#" data-toggle="dropdown">Tutorials<strong class="caret"></strong></a>
						<ul class="dropdown-menu" style="padding: 10px;">
							<li><a href="quickstart.php">Quickstart</a></li>
							<li><a href="basic.php">Basic Bot</a></li>
						</ul>
					</li>
					<li class="dropdown">
						<a class="dropdown-toggle" href="#" data-toggle="dropdown">Specs<strong class="caret"></strong></a>
						<ul class="dropdown-menu" style="padding: 10px;">
							<li><a href="game_spec.php">Game Spec</a></li>
							<li><a href="networking_spec.php">Networking Spec</a></li>
						</ul>
					</li>
					<li>
						<a href="downloads.php">Downloads</a>
					</li>
					<li>
						<a href="http://forums.halite.io">Forums</a>
					</li>
					<li>
						<a href="#" style="cursor: default;"></a>
					</li>
					<li class="dropdown">
						<a class="dropdown-toggle" href="#" data-toggle="dropdown">Login<strong class="caret"></strong></a>
						<ul class="dropdown-menu" style="padding: 10px; padding-bottom: 15px;">
							<?php include 'includes/login_form.php'; ?>
						</ul>
					</li>

					<li class="dropdown">
						<a class="dropdown-toggle" href="#" data-toggle="dropdown">Register<strong class="caret"></strong></a>
						<ul class="dropdown-menu" style="padding: 15px; padding-bottom: 15px;">
							<?php include 'includes/register_form.php'; ?>
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
