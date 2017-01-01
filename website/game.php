<!DOCTYPE HTML>
<html>
<head>
    <?php include 'includes/header.php'; ?>

    <title>Game</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
    <style type="text/css">   
        .glyphicon-refresh-animate {
            -animation: spin .7s infinite linear;
            -webkit-animation: spin2 .7s infinite linear;
        }
        @-webkit-keyframes spin2 {
            from {
                -webkit-transform: rotate(0deg);
            }
            to {
                -webkit-transform: rotate(360deg);
            }
        }
        @keyframes spin {
            from {
                transform: scale(1) rotate(0deg);
            }
            to {
                transform: scale(1) rotate(360deg);
            }
        }
    </style>

</head>
<body>
    <div id="container" class="container">
        <?php include 'includes/navbar.php'; ?>
        <div id="pageContent" class="pageContent text-center">
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
    <script src="script/game.js"></script>
</body>
</html>
