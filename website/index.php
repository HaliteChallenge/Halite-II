<?php
session_start();
if(isset($_SESSION['userID'])) {
    $config = parse_ini_file("../halite.ini", true);
    $mysqli = new mysqli($config['database']['hostname'],
        $config['database']['username'],
        $config['database']['password'],
        $config['database']['name']);

    if (mysqli_connect_errno()) {
        echo "<br><br>There seems to be a problem with our database. Reload the page or try again later.";
        exit();
    }
    if(count(mysqli_fetch_array(mysqli_query($mysqli, "SELECT * FROM User WHERE userID={$_SESSION['userID']} and isRunning=1"))) > 0) {
        header("Location: user.php?".$_SERVER['QUERY_STRING']);
    }
}
?>

<!DOCTYPE html>
<html lang="en">

<head>
    <?php include 'includes/header.php'; ?>

    <title>Homepage</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
    <style>
        #intro_blurb p {
            font-size: 1.5em;
        }
    </style>
</head>

<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="pageContent">
            <div class="row">
                <div class="col-sm-12">
                    <img src="assets/hero.png" style="margin-bottom: 21px; width: 100%">
                    <div id="intro_blurb">
                        <h1>Halite is an artificial intelligence programming challenge.</h1>
                        <p>Players control a bot using the programming language of their choice. Bots fight for control of a 2D grid and the bot with the most territory at the end wins. Victory will require micromanaging of the movement of your pieces, optimizing your bot's combat ability, and braving a branching factor billions of times higher than that of Go.</p>
                        <p>The simplest Halite bot is just 10 lines of code, and it takes less than a minute to get on the leaderboard. <a href="basics_quickstart.php">Interested?</a></p>
                        <p>This site is in beta. If you see any problems, please post on the <a href="http://forums.halite.io">forums</a>.</p>
                    </div>

                    <h1>Creators</h1>
                    <p>The Halite game was designed and implemented by <a href="https://www.twosigma.com">Two Sigma</a> for their annual summer programming competition. The current version of the competition is run with the help of <a href="http://tech.cornell.edu/">Cornell Tech</a>.</p>
                    
                    <div class="row">
                        <div class="col-sm-6">
                            <div class="text-center" style="margin-top: 10px; margin-bottom: 20.5px;"><a href="https://www.twosigma.com"><img src="assets/two_sigma.png" style="max-width: 100%; max-height: 70px;"></a></div>
                            <p>Two Sigma is a technology company dedicated to finding value in the world’s data. Since its founding in 2001, Two Sigma has built an innovative platform that combines extraordinary computing power, vast amounts of information, and advanced data science to produce breakthroughs in investment management, insurance and related fields.</p>
                        </div>

                        <div class="col-sm-6">
                            <div class="text-center" style="margin-top: 10px; margin-bottom: 20.5px;"><a href="https://tech.cornell.edu"><img src="assets/cornell_tech.png" style="max-width: 100%; max-height: 70px"></a></div>
                            <p>Cornell Tech brings together faculty, business leaders, tech entrepreneurs, and students in a catalytic environment to produce visionary results grounded in significant needs that will reinvent the way we live in the digital age.</p>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
    <script src="script/leaderTable.js"></script>
    <script src="script/index.js"></script>
</body>

</html>
