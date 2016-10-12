<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Quickstart</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
    <link href="style/learn.css" rel="stylesheet">
</head>
<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="row">
            <?php include 'includes/learn_sidebar.php'; ?>
            <div class="col-sm-9">
                <h1>Getting Started</h1>

                <h3>Register</h3>

                <p>Sign up for Halite by clicking "Login with Github" at the top right of this page.</p>

                <h3>Download a Starter Package</h3>

                <p>Navigate <a href="downloads.php">here</a> and download the starter package for your preferred language. Each starter package includes a simple bot and an API for talking with the game environment. 

                <h3>Submit the Starter Package</h3>

                <p>Click the "Submit" button in the navigation bar and upload the starter package zip file that you just downloaded. Our servers will compile your source and continuously play your bot against other contestants, ranking you in the process. To track your bot's progress and view its latest games, navigate to <a href="index.php">your homepage</a>. Want to put up another bot? Just submit new source code.</p>

                <p><b>Note:</b> If compilation of your source code fails, we will email you.</p>


                <h3>Need Help? Have Feedback?</h3>

                <p>Please post on <a href="http://forums.halite.io">the forums</a>.</p>
            </div>
        </div>
    </div>


    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
