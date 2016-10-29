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
        <div class="row" id="loginMessage" style="display: none;">
            <div class="col-md-12">
                <h1><a href="https://github.com/login/oauth/authorize?scope=user:email&client_id=2b713362b2f331e1dde3">Login</a> to see this page.</h1>
            </div>
        </div>
        <div class="row" id="noBotMessage" style="display: none;">
            <div class="col-md-12">
                <h1>You don't have a bot on the leaderboard!</h1>
            </div>
        </div>
        <div class="row" id="normalBody">
            <div class="col-lg-4 col-md-12">
                <div id="profileCard" class="text-center" style="padding: 21px; margin-bottom: 21px; border: 1px solid #0092a1;">
                    <img id="profileImage" style="border-radius: 100%; width: 100%; max-width: 200px;">
                    <h1 id="name" style="margin-top: 12px"></h1>
                    <h4 id="primary-info" style="line-height: 1.2em;"></h4>
                    <h5 id="secondary-info" style="line-height 1.2em; margin-bottom: 0px; color: gray;"></h5>
                </div>
                <div id="historyPanel" class="panel panel-primary">
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
            <div class="col-lg-8 col-md-12">
                <div class="text-center" style="display: none;" id="noGameMessage">
                    <span class="glyphicon glyphicon-warning-sign" style="font-size: 8em;"></span>
                    <h3>There are no games recorded yet.</h3>
                </div>
                <div id="gamePanel" class="panel panel-primary">
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
                    <button type="button" id="loadButton" class="btn btn-primary" style="width: 100%;">Load More</button>
                </div>
            </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
    <script src="script/user.js"></script>
</body>
</html>
