<!DOCTYPE html>
<html lang="en">

<head>
    <?php include 'includes/header.php'; ?>

    <title>Email Selection</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
    <style>
        h4 {
            font-weight: 400;
        }
    </style>
</head>

<body>
    <div class="container">
        <div id="messageBox"></div>
        <div class="pageContent">
            <div class="row">
                <div class="col-sm-12">
                    <div id="forms">
                        <h1>One Last Step...</h1>
                        <h4>You need to select your preferred email. We use your email to identify your organization (your university or your employer), using the domain of your email. We also send notifications about compilation errors to that email address.</h4>

                        <div style="margin-top: 20px; margin-bottom: 20px;">
                            <h4>Option 1: use your github's email, <b><span id="emailLoc"></span></b></h4>
                            <button id="githubSubmitButton" class="btn btn-primary">Yes</button>
                        </div>

                        <div>
                            <h4>Option 2: choose a new email</h4>
                            <input class="form-control" type="email" placeholder="Email" style="margin-bottom: 0px" id="firstField">
                            <input class="form-control" type="email" placeholder="Confirm Email" style="margin-bottom: 10px" id="secondField">
                            <button id="customSubmitButton" class="btn btn-primary">Submit</button>
                        </div>
                    </div>
                    <div id="waitMessage" style="display: none;">
                        <h2>We've sent you a verification email and are waiting for you to click the link in it. If you've already verified your email, head to our <a href="index.php">homepage</a>.</h2>
                        <h4>If you are having problems with registration, please email us at halite@halite.io.</h4>
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
