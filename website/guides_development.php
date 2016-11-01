<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Bot Development Guide</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
    <link href="style/learn.css" rel="stylesheet">
</head>
<body>
    <div>
        <?php include 'includes/navbar.php'; ?>
        <div>
            <?php include 'includes/learn_sidebar.php'; ?>
            <div>
                <h1>Bot Development Guide</h1>

           <h1>Development Guide</h1>
            <p>In this guide, we will detail a couple of best practices to follow when building your Halite bot.</p>
            <h3>Using a Log File</h3>
            <p>Stdout and stdin in are used to communicate with the game environment. As such, you cannot use functions like System.out.println, print(), or std::cout. Instead, print debugging information to a log file.</p>
            <h3>Local Bot Evaluation</h3>
            <p>Before submitting a new bot to the online leaderboard, we recommend running some games against the version of your bot that is currently on the leaderboard. If your new bot consistently wins, then put it up!</p>
            <h3>Disabling the Timeout Flag</h3>
            <p>When debugging latency issues with your bot, it can be helpful to disable game environment timeouts. To do so, append the -t flag to your environment command (e.g. ./environment -d 30 30 &quot;python3 MyBot.py&quot; &quot;python3 RandomBot.py&quot; -t).</p>
            <h3>Understanding Game Logs</h3>
            <p>When your bot times out or errors on our game servers, we save and display a log file with debugging information including the time your bot took each turn, its output each turn, and its final output from stdout and stderr.</p>
            <p>To find these log files, visit your homepage: <a href="https://halite.io/user.php">https://halite.io/user.php</a>. Just click the download log button to grab your error log for a game:</p>
            <p>///Screen shot with circle around error log\\\</p>
            <p>Here is an example error log:<br></p>
            <p>Each frame your bots output and the time that it took is shown:</p>
            <p>If the game environment kills your bot (likely due to timeout or error), we dump whatever is left in stdout and stderr.</p>
            <h3>Debugging with an IDE</h3>
            <p>There is a community contributed method for running a Halite bot from a custom debugger locally. More on this can be found here on the forums. </p>

            </div>
        </div>
    </div>


    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
