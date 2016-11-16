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
                    <h1>Email Selection</h1>
                    <p>Use the email <span id="emailLoc"></span>?<p/>
                    <button id="githubSubmitButton" class="btn btn-primary">Yes</button>

                    <input class="form-control" type="email" placeholder="Email" id="firstField">
                    <input class="form-control" type="email" placeholder="Confirm Email" id="secondField">
                    <button id="customSubmitButton" class="btn btn-primary">Submit</button>
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
