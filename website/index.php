<!DOCTYPE html>
<html>

<head>
	<title>Halite4</title>
	<link rel="stylesheet" type="text/css" href="style/index.css">
	<script src="lib/jquery.min.js"></script>s
	<script src="script/app.js"></script>

	<script type="text/javascript">
		$(document).ready(function() {
			$("#signIn").click(function() { goToSignIn(); });
			$("#signUp").click(function() { goToSignUp(); });
			$("#rankings").click(function() { goToRankings(); });
			$("#tutorial").click(function() { goToTutorial(); });
			$("#rules").click(function() { goToRules(); });
		});
	</script>
</head>

<body>
	<h1>Halite4</h1>
	<hr>
	
	<ul>
		<li><a href="#" id="signIn">Sign In</a></li>
		<li><a href="#" id="signUp">Sign Up</a></li>
		<li><a href="#" id="rankings">Rankings</a></li>
		<li><a href="#" id="tutorial">Tutorial</a></li>
		<li><a href="#" id="rules">Rules</a></li>
	</ul>
	
	<p>Halite is a game meant to be played by computers. Halite was created by Benjamin Spector and Michael Truell.</p>
</body>

</html>
