<!DOCTYPE HTML>
<html>
<head>
    <?php include 'includes/header.php'; ?>

    <title>Visualizer</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
</head>
<body>
    <div id="container" class="container">
        <?php include 'includes/navbar.php'; ?>
        <div id="pageContent" class="pageContent text-center">
            <div id="displayArea" class="col-sm-12 text-center">
                <span class="glyphicon glyphicon-cloud-upload" style="font-size: 10em;"></span>
                <h2 style="margin-top: 20px;">Drop or upload a replay file here</h2>

            </div>
            <div id='fileSelect' class="col-sm-12 text-center">
                <label for="filePicker" class="btn btn-primary">Select File</label>
                <input type="file" id="filePicker" accept=".hlt" style="display: none;">
            </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/lodash.js/4.17.2/lodash.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/seedrandom/2.4.0/seedrandom.min.js"></script>
    <script src="lib/xss.js"></script>
    <script src="script/general.js"></script>
    <script src="script/backend.js"></script>
    <script src="lib/pixi.min.js"></script>
    <script src="script/parsereplay.js"></script>
    <script src="script/visualizer.js"></script>
    <script src="script/localVisualizer.js"></script>
</body>
</html>
