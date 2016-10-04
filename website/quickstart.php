<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Quickstart</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
    <link href="style/learn.css" rel="stylesheet">
</head>
<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="row">
        	<?php include 'includes/learn_sidebar.php'; ?>
            <div class="col-sm-9">
                <h1>Quickstart</h1>

                <h3>Register</h3>
                <p>Sign up for Halite by clicking "Login with Github" at the top right of this page.</p>

                <h3>Download a Language Package</h3>
				<p>Halite is language agnostic. Bots talk to the game environment using stdin and stdout. We provide that a wrapper around that networking code the networking code necessary to talk to the game environment, some game constructs, and a basic Halite bots for several languages.</p>

				<p>Navigate <a href="downloads.php">here</a> to download a language package for your preferred language. The MyBot.py file contains the bot's <code>main()</code> function.

                <h3>Submit the Starter Bot</h3>
				<p>You can submit a zip file of your bot's source code to halite.io to be ranked and to see yourself in match against other contestants. The highest ranked bot at the end of the competition wins!</p>

				<p>Click the "Submit to Competition" button in the navigation bar and upload the language package zip file that you just downloaded. Your bot should appear on the leaderboard within a couple of minutes.</p>

                <h3>Need Help? Have Feedback?</h3>
                <p>Please post on <a href="http://forums.halite.io">the forums</a>.</p>
            </div>
        </div>
    </div>


    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
