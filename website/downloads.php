<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Downloads</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
</head>
<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="row">
            <div class="col-sm-12">
                <h1>Halite Downloads</h1>

                <h3>Game Environment</h3>
                <p>
                    This set of binaries was uploaded at <b>10:55am EST on August 2nd</b>. We discontinued the desktop visualizer (see below), so consequently the environment executable is the only file in this download.
                    <p>
                        <ul>
                            <li><a href="downloads/environment/HaliteEnvironment-Mac">Mac</a></li>
                            <li><a href="downloads/environment/HaliteEnvironment-Debian">Ubuntu (64 bit)</a></li>
                            <li><a href="downloads/environment/HaliteEnvironment-Windows.exe">Windows</a></li>
                        </ul>
                    </p>

                    <h3>Language Packages</h3>
                    <p>
                        This set of language packages was uploaded at <b>12:45pm EST on July 28th</b>. We added a Scala starter package!
                    </p>
                    <p>
                        <ul>
                            <li><a href="downloads/starterpackages/Halite-Python-Starter-Package.zip">Python 3</a></li>
                            <li><a href="downloads/starterpackages/Halite-Java-Starter-Package.zip">Java 7</a></li>
                            <li><a href="downloads/starterpackages/Halite-C++-Starter-Package.zip">C++ 11</a></li>
                            <li><a href="downloads/starterpackages/Halite-Rust-Starter-Package.zip">Rust 1.10</a></li>
                            <li><a href="downloads/starterpackages/Halite-Scala-Starter-Package.zip">Scala 2.10.4</a></li>
                        </ul>
                    </p>

                    <h3>Desktop Visualizer [DISCONTINUED]</h3>
                    <p>
                        This is the legacy desktop visualizer. We recommend using the <a href="game.php">web visualizer</a>, but until an alternative desktop visualizer is available we will continue to offer this for download. These binaries are no longer supported and are unlikely to be updated.
                    </p>
                    <p>
                        <ul>
                            <li><a href="downloads/visualizer/HaliteDesktopVis-Mac.zip">Mac</a></li>
                            <li><a href="downloads/visualizer/HaliteDesktopVis-Debian.zip">Ubuntu (64 bit)</a></li>
                            <li><a href="downloads/visualizer/HaliteDesktopVis-Windows.zip">Windows</a></li>
                        </ul>
                    </p>
                </div>
            </div>
        </div>

        <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
        <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
        <script src="script/backend.js"></script>
        <script src="script/general.js"></script>
    </body>
    </html>
