<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title></title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
</head>
<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="modal fade" id="notifModal" tabindex="-1" role="dialog" aria-labelledby="notifModalLabel">
            <div class="modal-dialog" role="document">
                <div class="modal-content">
                    <div class="modal-header" id="notifModalHeader">
                    </div>
                    <div class="modal-body" id="notifModalBody">
                    </div>
                </div>
            </div>
        </div>
        <div class="row" id="loginMessage" style="display: none;">
            <div class="col-md-12">
                <h2><a href="https://github.com/login/oauth/authorize?scope=user:email&client_id=2b713362b2f331e1dde3">Login</a> to see this page.</h2>
            </div>
        </div>
        <div class="row" id="noBotMessage" style="display: none;">
            <div class="col-md-12">
                <h2>This user doesn't currently have a bot on the leaderboard!</h2>
            </div>
        </div>
        <div class="row" id="normalBody">
            <div class="col-xl-6 col-lg-5 col-md-12">
                <div id="profileCard" class="panel panel-default text-center" style="padding: 21px; margin-bottom: 21px;">
                    <img id="profileImage" style="border-radius: 4px; width: 100%; max-width: 200px;">
                    <h1 id="name" style="margin-top: 12px"></h1>
                    <p id="primaryInfo" style="line-height: 1.2em; font-size: 20px;"></p>
                    <p id="secondaryInfo" style="line-height 1em; margin-bottom: 0px; font-size: 15px;"></p>
                </div>
                <div id="notifsPanel" style='display: none;' class="panel panel-default">
                    <div class="panel-heading">
                        <h3 class="panel-title">Events</h3>
                    </div>
                    <table class="table" id="notifsTable">
                        <tbody id="notifsTableBody">
                        </tbody>
                    </table>
                </div>
                <div id="historyPanel" class="panel panel-default">
                    <div class="panel-heading">
                        <h3 class="panel-title">Recent History</h3>
                    </div>
                    <table class="table" id="historyTable">
                        <thead>
                            <tr id="historyTableHeader">
                                <th>Version</th>
                                <th>Final Ranking</th>
                                <th>Games Played</th>
                            </tr>
                        </thead>
                        <tbody id="historyTableBody">
                        </tbody>
                    </table>
                </div>
            </div>
            <div class="col-xl-6 col-lg-7 col-md-12">
                <div class="text-center" style="display: none;" id="noGameMessage">
                    <span class="glyphicon glyphicon-warning-sign" style="font-size: 8em;"></span>
                    <h3>There are no games recorded yet.</h3>
                </div>
                <div id="gamePanel" class="panel panel-default">
                    <div class="panel-heading">
                        <h3 class="panel-title">Game Feed</h3>
                    </div>
                    <table class="table" id="gameTable">
                        <thead>
                            <tr id="gameTableHeader">
                            </tr>
                        </thead>
                        <tbody id="gameTableBody">
                        </tbody>
                    </table>
                    <button type="button" id="loadButton" class="btn btn-default" style="width: 100%;">Load More</button>
                </div>
            </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="lib/xss.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
    <script src="script/user.js"></script>
</body>
</html>
