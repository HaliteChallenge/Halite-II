<!DOCTYPE html>
<html lang="en">

<head>
    <?php include 'includes/header.php'; ?>

    <title>Email Selection</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
</head>

<body>
    <div class="container">
        <div id="messageBox"></div>
        <div class="pageContent">
            <div class="row">
                <div class="col-sm-12">
                    <div id="forms">
                        <h1>Email Selection</h1>
                        <h3>Use your github's email: <b><span id="emailLoc"></span></b>?</h3>
                        <button id="githubSubmitButton" class="btn btn-primary">Yes</button>

                        <h3>Or, choose a custom email:</h3>
                        <input class="form-control" type="email" placeholder="Email" id="firstField">
                        <input class="form-control" type="email" placeholder="Confirm Email" id="secondField">
                        <button id="customSubmitButton" class="btn btn-primary">Submit</button>
                        </div>
                    </div>
                    <div id="waitMessage" style="display: none;">
                        <h2>We are waiting for you to verify your email. If you have already verified your email, head to our <a href="index.php">hompeage</a>.</h2>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/email.js"></script>
</body>

</html>
