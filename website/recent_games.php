<!DOCTYPE html>
<html lang="en">

<head>
    <?php include 'includes/header.php'; ?>

    <title>Recent Games</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
</head>

<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="pageContent">
          <div class="row">
            <div class="col-xl-12 col-lg-12 col-md-12">
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
                                <th>Time</th><th>Players</th><th>Dimensions</th><th>View</th>
                            </tr>
                        </thead>
                        <tbody id="gameTableBody">
                        </tbody>
                    </table>
                </div>
            </div>
          </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="lib/xss.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
    <script src="script/recent_games.js"></script>
</body>

</html>
