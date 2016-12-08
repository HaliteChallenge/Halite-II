
<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="google-site-verification" content="UzLAOvN92N2iaw_7HcFXSOc_M-WIe3KFXaozuaNsZo4" />

    <title>Homepage</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
</head>

<body>
    <div class="container">
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
                    <li>
  <a href="basics_quickstart.php">Learn</a>
</li>
<li>
  <a href="downloads.php">Download</a>
</li>
<li>
  <a href="leaderboard.php">Rankings</a>
</li>
<li>
    <a href="local_visualizer.php">Visualize</a>
</li>
<li>
  <a href="http://forums.halite.io">Forums</a>
</li>
<li>
  <a href="about.php">About</a>
</li>                    <li>
                      <a href="https://github.com/login/oauth/authorize?scope=user:email&client_id=2b713362b2f331e1dde3">Login with Github</a>
                    </li>
                </ul>
            </ul>
            <form id="submitForm">
                <ul class="nav navbar-nav navbar-right loggedIn" id="logoutNav">
                    <li>
  <a href="basics_quickstart.php">Learn</a>
</li>
<li>
  <a href="downloads.php">Download</a>
</li>
<li>
  <a href="leaderboard.php">Rankings</a>
</li>
<li>
    <a href="local_visualizer.php">Visualize</a>
</li>
<li>
  <a href="http://forums.halite.io">Forums</a>
</li>
<li>
  <a href="about.php">About</a>
</li>                    <li><a href="#" id="submitButton">Submit</a><input type="file" id="myFile" name="botFile"></li>
                    <li><a href="#" id="logoutButton">Logout</a></li>
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
                <h3>Make sure that you aren't using stdout or stdin (print, cout, System.out.print, etc), which will cause your bot to fail. Instead, use <a href="guides_development.php">a log file</a>.</h3>
                <h3>Upload a <b>zip file</b> of your source code.</h3>
                <h3>Once we have compiled your code, you will get an email notification.</h3>
                <button id="submitModalButton" type="button" class="btn btn-primary">Submit!</button>
            </div>
        </div>
    </div>
</div>
        <div class="pageContent">
            <div class="row">
                <div class="col-sm-12">
                    <img src="assets/hero.png" style="margin-bottom: 21px; width: 100%">
                    <div id="intro_blurb">
                        <h1>Halite is an artificial intelligence programming challenge.</h1>
                        <p>Players control a bot using the programming language of their choice. Bots fight for control of a 2D grid. The bot with the most territory at the end wins. Victory will require micromanaging of the movement of your pieces, optimizing your bot's combat ability, and braving a branching factor billions of times higher than that of Go.</p>
                        <p>The simplest Halite bot is just 10 lines of code, and it takes less than a minute to get on the leaderboard. <a href="basics_quickstart.php">Interested?</a></p>
                        <div id="gameReplay" class="text-center"></div>
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

                    <h1>Sponsors</h1>
                    <p>Halite is grateful for the support of many strong tech partners!</p>
                    
                    <div class="text-center" style="margin-top: 10px; margin-bottom: 20.5px;">
                        <a href="https://www.vettery.com/"><img src="assets/vettery.png" style="max-width: 100%; max-height: 70px; margin-right: 40px;"></a>
                        <a href="https://www.aminoapps.com/"><img src="assets/amino.png" style="max-width: 100%; max-height: 70px;"></a>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/seedrandom/2.4.0/seedrandom.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
    <script src="lib/pixi.min.js"></script>
    <script src="script/parsereplay.js"></script>
    <script src="script/visualizer.js"></script>
    <script src="script/index.js"></script>
</body>

</html>
