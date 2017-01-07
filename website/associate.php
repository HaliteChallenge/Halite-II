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
        <div class="pageContent">
            <div class="row">
                <div class="col-sm-12">
                    <div id="messageBox"></div>
                    <div id="associateMessage">
                        <h1>Associate With a Valid Institution</h1>
                        <h4>Please enter your Association Information.</h4>
                        <h4>We’ll use the domain of your email address to identify your university or company so you can easily see how you rank against others in your organization. We’ll also email you about bot submission events like compilation errors. You can opt out of these at any time.</h4>
                    </div>

                    <label for="selectionLevel">Select your level:</label>
                    <select class="form-control" type="level" style="margin-top: 0px;max-width: 400px;margin-bottom: 20px;" id="selectionLevel">
                        <option>High-School</option>
                        <option>Undergraduate</option>
                        <option>Graduate</option>
                        <option selected="selected">Professional</option>
                    </select>       

                    <div id="highSchoolItems">
                        <label for="selectionHighSchool">Select your High School:</label>
                        <select class="form-control" type="level" style="margin-top: 0px;max-width: 400px;margin-bottom: 20px;" id="selectionHighSchool">
                        </select>
                    </div>

                    <div id="forms">
                        <label for="firstField" style="margin-top: 0px">Please enter your e-mail:</label>
                        <input class="form-control" type="email" placeholder="Email" style="max-width: 400px;" id="firstField">
                        <input class="form-control" type="email" placeholder="Confirm Email" style="margin-bottom: 10.5px; max-width: 400px;" id="secondField">
                        <button id="customSubmitButton" class="btn btn-primary">Submit</button>
                    </div>

                    <div id="waitMessage" style="display: none;">
                        <h2>We've sent you a verification email and are waiting for you to click the link in it. This email may land in your spam folder.</h2>
                        <h4>If you've already verified your email, head to our <a href="index.php">homepage</a>. If you're having problems with registration, please email us at halite@halite.io.</h4>
                    </div>
                </div>
            </div>
        </div>
        <?php include 'includes/footer.php'; ?>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/associate.js"></script>
</body>

</html>
