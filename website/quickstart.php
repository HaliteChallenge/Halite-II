<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>Quickstart</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/index.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
			<div class="col-sm-12">
				<h1>Quickstart</h1>

				<div class="videoWrapper">
					<iframe width="420" height="315" src="http://www.youtube.com/embed/2_N6ZcOioQI"></iframe>
				</div>

				<div class="videoWrapper">
					<iframe width="420" height="315" src="http://www.youtube.com/embed/0dlrkXPPTdA"></iframe>
				</div>

				<h3>Register</h3>
				<p>Sign up for Halite with a Two Sigma email. Click on the link in the verification email that we sent you. Now log in to your account.</p>

				<h3>Download a Starter Package</h3>
				<p>Halite is language agnostic; bots talk to the game environment using stdin and stdout. However, this requires some boilerplate code. We provide that boilerplate and a couple of basic Halite bots in the form of language "starter packages." Navigate <a href="downloads.php">here</a> to download the starter package for your preferred language. If we don't currently support your preferred language, you can send us an email at <a href="mailto:halite@halite.io">halite@halite.io</a>. You may also write your own starter package. The networking protocol that the game environment uses to interface with the starter packages is detailed <a href="api_spec.php">here</a>. If you write your own stater package, please contact us so we can publish it on this site.</p>

				<h3>Submit the Starter Bot</h3>
				<p>Users may submit their bots to Halite's website in order to qualify for the playoffs, to get their ranking, and to see their bot in match against others. In order to submit the random bot given in the starter packages, click the "Submit to Competition" button and upload a zip of the files inside your starter package's directory. Your bot should appear on the leaderboard within a couple of minutes.</p>

				<h3>Need Help? Have Feedback?</h3>
				<p>Don't hesitate to send us an email at <a href="mailto:halite@halite.io">halite@halite.io</a> or find either Michael Truell or Ben Spector on the 16th floor of 100 Avenue of the Americas. We work right next to the Hacker Lab.</p>
			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
					<li class="next"><a href="tutorial.php">Tutorial <span aria-hidden="true">&rarr;</span></a></li>
				</ul>
			</div>
		</div>
	</footer>

	<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
	<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
	<script src="script/backend.js"></script>
	<script src="script/general.js"></script>
</body>
</html>
