<!DOCTYPE html>
<html lang="en">

<head>
    <?php include 'includes/header.php'; ?>

    <title>Secret Registration</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
</head>

<body>
    <div class="container">
        <div id="messageBox"></div>
        <div class="pageContent">
            <div class="row">
                <div class="col-sm-12">
                    <div class="panel panel-primary">
                        <div class="panel-heading">
                            <h3 class="panel-title">Halite Secret Registration</h3>
                        </div>
                        <div style="padding: 16px; padding-bottom: 24px;">
                            <?php include 'includes/register_form.php'; ?>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
    <script src="script/secret_registration.js"></script>
</body>

</html>
