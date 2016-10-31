<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Getting Started</title>

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
                <h1>Getting Started</h1>
                <p>In this short tutorial, we will explain how to submit your first bot and get your name on the Halite leaderboard.</p>
                
                <h3>Register using Github</h3>

                <p>You will need a Github account in order to participate in the Halite tournament. If you don't have a Github account, please create a new account <a href="https://github.com/join"> here</a>. Sign up for Halite by clicking "Login with Github" at the top right of this page.</p>

                <h3>Download a Starter Package</h3>

                <p>Download a starter package for your language of choice from the <a href="downloads.php">downloads page</a>.</p>
                
                <p>Each starter package consists of a zip file that includes: (a) one code template for your your first bot (the MyBot file), (b) one code sample for a random bot (the RandomBot file), (c) some support libraries for the bot and (d) some tools to test your bot locally.</p> 

                <h3>Getting on the Leaderboard</h3>

                <p>To get on the leaderboard, you need to submit a bot to the Halite tournament.</p>
                <p>Click the "Submit" button in the navigation bar. In general, you need to package your submission as a zip file that contains everything needed to run your bot (your bot’s source code, any supplemental libraries, etc.). For your first submission, you can simply upload the zipped starter package that you just downloaded. You will receive an email notification that your submission has been received.</p>
                <p>The Halite servers will compile the source code of your submission and continuously play your bot against other contestants, which will generate your leaderboard ranking. If the compilation of your source code fails, we will email you.</p>
                <p>To track your bot's progress, navigate to your <a href="user.php">homepage</a>.</p>

                <h3>Next Steps</h3>
                <p>Congratulations. You are now an official contestant!</p>
                <p>It may be up to <b>10 minutes</b> before your bot has played a few games. In the meantime, move onto <a href="basics_intro_halite.php">Introducing Halite</a>.</p>

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
