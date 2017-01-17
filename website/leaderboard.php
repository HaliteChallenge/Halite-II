<!DOCTYPE html>
<html lang="en">

<head>
    <?php include 'includes/header.php'; ?>

    <title>Rankings</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
</head>

<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="pageContent">
            <div class="row">
                <div class="col-sm-12">
                    <div class="row" style="margin-top: 21px; margin-bottom: 10px;">
                        <div class="col-sm-9">
                            <h1 id="leaderHeading" style="margin-top: 0px;">Leaderboard</h1>
                            <p>There are <span id="numUsers">1366</span> users on the Halite leaderboard. Want to join them? Visit our <a href="basics_quickstart.php">getting started guide</a>.</p>
                        </div>
                        <div class="col-sm-3">
                            <div class="input-group">
                               <input type="text" id="usernameField" class="form-control" placeholder="Username">
                               <span class="input-group-btn">
                                    <button class="btn btn-default" style="border-width: 2px;" id="usernameSubmitButton" type="button">Find</button>
                               </span>
                            </div>
                        </div>
                    </div>
                    <div class="panel panel-default">
                        <?php include 'includes/leaderTable.php'; ?>
                    </div>

                    <hr>

                    <div id="footer">
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
    <script src="script/leaderTable.js"></script>
    <script src="script/leaderboard.js"></script>
</body>

</html>
