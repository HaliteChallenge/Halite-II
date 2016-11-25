<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Using Package Managers</title>

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
                <h1>Using Package Managers</h1>
                <p>The following package managers are included on the Halite game servers:</p>

                <ul>
                    <li>pip3</li>
                    <li>bundler</li>
                    <li>npm</li>
                </ul>

                <p>The environment managers are included on the Halite game servers:</p>

                <ul>
                    <li>virtualenv</li>
                </ul>

                <p>
                    If you would like us to add another package manager or environment manager to this list, post on the <a href="http://forums.halite.io">forums</a>.
                </p>

                <p>To make calls your package manager, create an "install.sh" file in the root directory of your zip file submission and put any bash commands in it. Before compilation, that file will be executed with internet and write access to its current directory. It may run for a maximum of 10 minutes.</p>

                <p>Note: All libraries have to be installed locally.</p>
                
                <h3>Examples</h3>

                <p>Here is an example of an "install.sh" file that installs the <a href="http://keras.io/">the keras library</a> using virtualenv:</p>
                <span data-gist-id="f7c873227c5b96c72b4374bf203acae1"></span>

                <p>An install.sh for npm (just what you would expect):</p>
                <span data-gist-id="510f9cd386ac5f00bf643a51a1c01df5"></span>

            </div>
        </div>
        <?php include 'includes/footer.php'; ?>
    </div>


    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/gist-embed/2.4/gist-embed.min.js"></script>
    <script src="https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
