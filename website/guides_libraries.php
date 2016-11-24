<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Using 3rd Party Libraries</title>

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
                <h1>Using 3rd Party Libraries</h1>
                <p>If you add a bash file named install.sh to the same directory as your bot source, it will be run with internet access and write access to its current directory before any bot compilation. It may run for a maximum of 10 minutes.</p>

                <h3>Package Managers</h3>
                <p>You must install all libraries locally, even if through a package manager. Because of this, package managers that only support global installation (such as apt-get) may not be used.</p>
                <p>The following package managers are installed ahead of time on the Halite game servers:</p>
                    <ul>
                        <li>pip3</li>
                        <li>bundler</li>
                        <li>npm</li>
                    </ul>
                <p>
                    If you would like us to add another package manager to this list, post on the <a href="http://forums.halite.io">forums</a>.
                </p>

                <p>Here is an example install.sh file for a bot that uses <a href="http://www.numpy.org/">the numpy library</a>:</p>
                <span data-gist-id="039bb1e4916ffb012d1ef83b36572dee"></span>
                <p>It's just one line!</p>

                <h3>Compilation</h3>
                <p>If your library isn't on a package manager that supports local installation, you are going to have to compile it on our game servers. Include the source of you library in your bot's zip file and put compilation instructions in the install.sh file.</p>
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
