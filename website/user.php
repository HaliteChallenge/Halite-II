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
            <div class="col-md-5">
                <div class="panel panel-primary">
                    <div class="panel-body text-center">
                        <h1 id="name" style=""></h1>
                        <h4 id="primary-info" style=""></h4>
                        <div id="secondary-info" style="color: gray;"></div>
                    </div>
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
            <div class="col-md-7">
                <div class="panel panel-primary">
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
