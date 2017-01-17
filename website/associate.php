<!DOCTYPE html>
<html lang="en">

<head>
    <?php include 'includes/header.php'; ?>

    <title>Association</title>

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
                        <h1>Share Your Affiliation</h1>
                        <h4>Curious to see how your organization stacks up against the others? Want to see who else from your company or school is playing Halite?</h4>
                        <h4>Enter your work/school information below and we'll match you with your organization. This way you can see your ranking on your team's leaderboard as well as your ranking overall. May the best bot win!</h4>
                    </div>


                    <div id="levelItems">
                        <label for="selectionLevel">Select your level:</label>
                        <select class="form-control" type="level" style="margin-top: 0px;max-width: 400px;margin-bottom: 20px;" id="selectionLevel">
                            <option>High School</option>
                            <option>Undergraduate</option>
                            <option>Graduate</option>
                            <option selected="selected">Professional</option>
                        </select>       
                    </div>

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
        <br/><br/>
        <div id="missingAssociation">
            <p><center><small>Is your organization missing from our selection? Click <a href="mailto:halite@halite.io?Subject=New%20Organization%20Request" target="_top">here</a> to e-mail us with the name and we will add it promptly.</small></center></p>
        </div>
        <?php include 'includes/footer.php'; ?>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/associate.js"></script>
</body>

</html>
