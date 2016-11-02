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
        p {
            line-height: 1.6em;
        }

    </style>
</head>

<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="pageContent">
            <div class="row">
                <div class="col-sm-12">
                    <img src="assets/hero.png" style="width: 100%">
                    <h1>Introducing Halite</h1>
                    <b><p>Disclaimer: Halite will be launching on November 2nd. The site is currently in beta.</p></b>
                    <p>Halite is a multiplayer turn-based strategy game played on a rectangular grid. The objective of the game is to take over the entire grid and eliminate the other players in the game.</p>
                    <p>Halite is really a programming game. As a human player, you don't interact with the game directly. Rather, you program a bot that will play games on your behalf.</p>
                    <p>Halite continuously and randomly organizes "games" between bots and uses the outcome to rank bots and create a leaderboard for players.</p>
                    <p>The simplest Halite bot is just 10 lines of code, and it takes less than a minute to get on the leaderboard. <a href="basics_quickstart.php">Interested?</a></p>

                    <h1>Top Rankings</h1>
                    <p>Here are some of the best bots playing Halite right now. Click on their usernames to see some of their recent games. Watching them might just give you some stratagy ideas.</p>
                    <div class="panel panel-default">
                        <?php include 'includes/leaderTable.php'; ?>
                    </div>

                    <h1>Creators</h1>
                    <p>The Halite game was designed and implemented by <a href="https://twosigma.com">Two Sigma</a> for their annual summer programming competition. The current version of the competition is run with the help of <a href="http://tech.cornell.edu/">Cornell Tech</a>.</p>
                    
                    <div class="row">
                        <div class="col-sm-6">
                            <div class="text-center" style="margin-top: 10px; margin-bottom: 20.5px;"><a href="https://twosigma.com"><img src="assets/two_sigma.png" style="max-width: 100%; max-height: 70px;"></a></div>
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
