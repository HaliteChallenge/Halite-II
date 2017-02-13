<?php
session_start();
$config = parse_ini_file("../halite.ini", true);
if(isset($_SESSION['userID'])) {
    $mysqli = new mysqli($config['database']['hostname'],
        $config['database']['username'],
        $config['database']['password'],
        $config['database']['name']);

    if (mysqli_connect_errno()) {
        echo "<br><br>There seems to be a problem with our database. Reload the page or try again later.";
        exit();
    }
    if(count(mysqli_fetch_array(mysqli_query($mysqli, "SELECT * FROM User WHERE userID={$_SESSION['userID']} and isEmailGood=0"))) > 0) {
        header("Location: associate.php");
    }
}
?>
<nav class="navbar navbar-default" id="navvy">
    <div class="container-fluid">
        <div class="navbar-header">
            <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
                <span class="sr-only">Toggle navigation</span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
            </button>
            <a class="navbar-brand" style="padding: 10px" href="index.php"><img src="assets/full_logo.png" style="height: 100%;"></a>
        </div>
        <div id="navbar" class="navbar-collapse collapse">
            <ul class="nav navbar-nav navbar-right loggedOut" id="loginNav">
                <ul class="nav navbar-nav">
                    <?php include 'includes/dropdowns.php'; ?>
                    <li>
                      <a href="https://github.com/login/oauth/authorize?scope=user:email&client_id=2b713362b2f331e1dde3">Login with Github</a>
                    </li>
                </ul>
            </ul>
            <form id="submitForm">
                <ul class="nav navbar-nav navbar-right loggedIn" id="logoutNav">
                    <?php include 'includes/dropdowns.php'; ?>
                    <?php if(!isset($config["compState"]["closeSubmissions"]) || !$config["compState"]["closeSubmissions"]): ?>
                    <li><a href="#" id="submitButton">Submit</a><input type="file" id="myFile" name="botFile"></li>
                    <?php endif; ?>
                    <li><a href="#" id="logoutButton">Logout</a></li>
                    <li><a href="associate.php">Associate</a></li>
                </ul>
            </form>
        </div>
    </div>
</nav>

<div id="messageBox"></div>

<div class="modal fade" id="submitModal" tabindex="-1" role="dialog" aria-labelledby="myModalLabel">
    <div class="modal-dialog" role="document">
        <div class="modal-content">
            <div class="modal-header">
                <h1 class="modal-title" id="myModalLabel">Halite Submission Instructions</h1>
            </div>
            <div class="modal-body">
                <h3>Name your main file <b>MyBot</b> with an appropriate file extension (e.g. MyBot.java).</h3>
                <h3>Make sure that you aren't using stdout or stdin (print, cout, System.out.print, etc), which will cause your bot to fail. Instead, use <a href="advanced_development.php">a log file</a>.</h3>
                <h3>Upload a <b>zip file</b> of your source code.</h3>
                <h3>Once we have compiled your code, you will get an email notification.</h3>
                <button id="submitModalButton" type="button" class="btn btn-primary">Submit!</button>
            </div>
        </div>
    </div>
</div>
