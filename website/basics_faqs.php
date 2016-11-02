<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>FAQs</title>

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
           	<h1>Frequently Asked Questions</h1>
        
                <h2>About</h2>
        
                <h3>What is Halite?</h3>
                <p>Halite (the game) is a multiplayer turn-based strategy game played on a rectangular grid; the objective of the game is to take over the entire grid and eliminate the other players in the game.<br>Halite (the tournament) is an online programming competition where human coders write bots that play the Halite game against each other.</p>

                <h3>Where does the game come from? Who invented it?</h3>
                <p>The Halite game was created by <a href="https://github.com/Sydriax">Benjamin Spector</a> and <a href="https://github.com/truell20">Michael Truell</a> during a summer internship at Two Sigma in 2016.<br>The game format is inspired by the 2011 Ants AI Challenge sponsored by Google.</p>

                <h3>Why did you invent it?</h3>
                <p>Halite’s creators heard about the Ants AI Challenge a few years too late :-(<br>The couldn’t find any other competitions quite like it, so they decided to create one.</p>

                <h2>About the Tournament</h2>

                <h3>What's the goal of the competition?</h3> 
                <p>The goal of Halite is to provide a fun and deep testbed to try new ideas and compete in problem-solving. Furthermore, Halite is being used for recruitment of talent by its sponsors.</p>

                <h3>Who is running the tournament?</h3>
                <p>The Halite game has been designed and implemented by <a href="https://www.twosigma.com/">Two Sigma</a>, a highly-innovative, technological investment firm based in New York City.<br>The Halite tournament is jointly run with <a href="http://tech.cornell.edu/">Cornell Tech</a>, a new graduate school that brings together faculty, business leaders, tech entrepreneurs, and students in a catalytic environment to produce visionary results grounded in significant needs that will reinvent the way we live in the digital age.</p>

                <h3>How long is the tournament?<h3>
                <p>The tournament will run from November 2, 2016 to February 2, 2017.</p>

                <h3>How are rankings computed?<h3>
                <p>Rankings are based on the outcome of organized games where bots play against each other. A good analogy is the <a href="https://en.wikipedia.org/wiki/Elo_rating_system">Elo rating system</a> used for chess.<br>More precisely, rankings are computed using a Bayesian algorithm variant of the <a href="https://en.wikipedia.org/wiki/Glicko_rating_system">Glicko system</a>, specifically using the TrueSkill Python library available <a href="https://github.com/sublee/trueskill">here</a>.</p>

                <h3>How are the winners decided?<h3>
                <p>Winners are simply the highest ranked players on the <a href="https://halite.io/leaderboard.php">leaderboard</a> at the end of the competition. So, submit early and often!</p>

                <h3>Is there a prize for the winners?<h3>
                <p>Pride! Bragging rights! Internet royalty!<br>The results of the competition will be officially announced with link to best players Github profiles.</p>

                <h3>What information do you store about me?</h3> 
                <p>We store the email, username, and unique identifier that Github provides when you login to the halite.io website via Github OAuth.</p>

                <h2>Bot Programming</h2>

                <h3>Do I need to be a programmer to play the game?<h3>
                <p>Yes. Halite is a programming competition. You need to program a bot that will play the game in the Halite tournament.<br>However, you definitely don’t have to be a very good programmer to play Halite effectively. Success is more about coming up with a good strategy to play the game than coding this strategy expertly.</p>

                <h3>What languages does the game support?<h3>
                <p>Any and all! If the language can read from stdin and print to stdout, we can support it.<br>We provide out-of-the-box starter packages for the following languages: Python, Java and C++. See <a href="https://halite.io/downloads.php">here</a> for our growing list of starter packages.<br>We’re counting on the community to add support for as many languages as people want. Visit <a href="https://halite.io/advanced_writing_sp.php">this page</a> for more information on writing your own starter package and the protocol used by the game environment to talk to your bot.

                <h3>How do I submit my bot?</h3>
                <p>To submit your bot, you'll first need to zip your source code. Then, after signing in, click the "Submit" button on the top-right part of the page. Then, simply select your zipped source code to submit.</p>

                <h2>The Code behind the Tournament</h2>

                <h3>Is the code open source?<h3>
                <p>Yes.<br>Check out our <a href="https://github.com/HaliteChallenge/Halite">Github repo</a> and you are also welcome (even encouraged) to open issues and/or submit pull requests.</p>

                <h3>How do I contribute to the game and/or report an issue?</h3>
                <p>You can open an issue or submit a pull request on our <a href="https://github.com/HaliteChallenge/Halite">Github Repo</a>. If you are looking for things to do, checkout our <a href="https://github.com/HaliteChallenge/Halite/issues">open issues</a> or our <a href="https://github.com/HaliteChallenge/Halite/projects/1">project board</a>.</p>

                <h2>Other</h2>

                <h3>I cannot find an answer to my question, what do I do?</h3>
                <p>Please check/post on the <a href="forums.halite.io">Halite forums</a> or contact-us at <a href="mailto:halite@halite.io">halite@halite.io</a>.
                
            </div>
        </div>
    </div>


    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
