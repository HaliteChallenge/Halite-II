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
        header("Location: user.php");
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
                    <h1>Introducing Halite</h1>
                    <p>Halite is a multiplayer turn-based strategy game played by computer programs on a rectangular grid. Each player’s objective in Halite is to take over the entire map and eliminate the other players in the game.</p>
                    <p>Bots must use their territory to gain strength and their strength to gain territory, move intelligently on both the micro and macro scales, and brave a branching factor of ~10<sup>1750</sup> to play effectively.</p>
                    <p>The simplest halite bot is just 10 lines of code, and it takes less than a minute to get on the leaderboard. Interested? <a href="basics_quickstart.php">Get started building a bot</a>.</p>
                    <h1>Top Rankings</h1>
                    <p>Here are some of the best bots playing Halite right now. Click on their usernames to see some of their recent games. Watching them might just give you some stratagy ideas.</p>
                    <div class="panel panel-default">
                        <?php include 'includes/leaderTable.php'; ?>
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
