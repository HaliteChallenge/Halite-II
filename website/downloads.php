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
                <h1>Downloads</h1>

                <h2>Starter Packages</h2>
                <p>
                    This set of starter packages was uploaded on November 6th. We fixed a minor bug with the getDistance() method in the Python starter package.
                </p>
                <p>
                    <ul>
                        <li><a href="downloads/starterpackages/Halite-Python-Starter-Package.zip">Python 3</a></li>
                        <li><a href="downloads/starterpackages/Halite-Java-Starter-Package.zip">Java 7</a></li>
                        <li><a href="downloads/starterpackages/Halite-C++-Starter-Package.zip">C++ 11</a></li>
                        <li><a href="downloads/starterpackages/Halite-C%23-Starter-Package.zip">C# 6.0</a></li>
                        <li><a href="downloads/starterpackages/Halite-Rust-Starter-Package.zip">Rust 1.10</a></li>
                        <li><a href="downloads/starterpackages/Halite-Scala-Starter-Package.zip">Scala 2.10.4</a></li>
                    </ul>
                </p>

                <h2>Game Environment</h2>
                <p>The environment is on version <b>1.0</b>. This version of the environment was posted on November 2nd.</p>

                <h3>Linux/macOS</h3>
                <p>Execute:</p>
                <pre><code>sudo sh -c "$(curl -fsSL https://raw.githubusercontent.com/HaliteChallenge/Halite/master/environment/install.sh)"</code></pre>
                <p>Now, the <code>halite</code> command will run the environment.</p>

                <h3>Windows</h3>
                <p>Download <a href="downloads/environment/halite.exe">halite.exe</a>.</p>
                
                <h3>Building from Source</h3>
                <p>Extract <a href="downloads/environment/HaliteEnvironment-Source.zip">this archive</a>.</p>
                <p>If you are on Unix, run <code>make</code> and <code>sudo make install</code> inside the extracted folder. The <code>halite</code> command will now run the environment.</p>
                <p>If you are on Windows, run the "make.bat" file. The file named "halite.exe" is your new environment binary.</p>
            </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
