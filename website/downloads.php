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
                    On December 31st, we patched the version of the Java starter package that we had released on December 30th.
                </p>
                <h3>Official Packages</h3>
                <ul>
                    <li><a href="downloads/starterpackages/Halite-Python-Starter-Package.zip">Python 3</a></li>
                    <li><a href="downloads/starterpackages/Halite-Java-Starter-Package.zip">Java 8</a></li>
                    <li><a href="downloads/starterpackages/Halite-C++-Starter-Package.zip">C++ 11</a></li>
                </ul>
                <h3>Community Packages</h3>
                <ul>
                    <li><a href="downloads/starterpackages/Halite-C%23-Starter-Package.zip">C# 6.0</a></li>
                    <li><a href="downloads/starterpackages/Halite-Rust-Starter-Package.zip">Rust 1.10</a></li>
                    <li><a href="downloads/starterpackages/Halite-Scala-Starter-Package.zip">Scala 2.10.4</a></li>
                    <li><a href="downloads/starterpackages/Halite-Ruby-Starter-Package.zip">Ruby 2.3.1</a></li>
                    <li><a href="downloads/starterpackages/Halite-Go-Starter-Package.zip">Go 1.6</a></li>
                    <li><a href="downloads/starterpackages/Halite-PHP-Starter-Package.zip">PHP 7.0</a></li>
                    <li><a href="downloads/starterpackages/Halite-JavaScript-Starter-Package.zip">Node.js (JavaScript) 7.1.0</a></li>
                    <li><a href="downloads/starterpackages/Halite-OCaml-Starter-Package.zip">OCaml 4.01.0</a></li>
                    <li><a href="downloads/starterpackages/Halite-Clojure-Starter-Package.zip">Clojure 1.8.0</a></li>
                    <li><a href="downloads/starterpackages/Halite-C-Starter-Package.zip">C</a></li>
                    <li><a href="downloads/starterpackages/Halite-Julia-Starter-Package.zip">Julia 0.5.0</a></li>
                </ul>
                </p>

                <h2>Game Environment</h2>
                <p>The environment is on version <b>1.1</b>. This version of the environment was posted on December 9th.</p>

                <h3>Linux/macOS</h3>
                <p>Execute:</p>
                <pre><code>sh -c "$(curl -fsSL https://raw.githubusercontent.com/HaliteChallenge/Halite/master/environment/install.sh)"</code></pre>
                <p>You should see a file titled "halite" in your current directory. This is the game environment. Put it in the root directory of your starter package.</p>

                <h3>Windows</h3>
                <p>Download the halite.exe executable from <a href="https://ci.appveyor.com/project/Sydriax/halite/build/artifacts">here</a>. Put it in the root directory of your starter package.</p>

                <h3>Building from Source</h3>
                <p>Extract <a href="downloads/environment/HaliteEnvironment-Source.zip">this archive</a>.</p>
                <p>If you are on Unix, run <code>make</code> inside the extracted folder. The halite binary will now run the environment.</p>
                <p>If you are on Windows, run the "make.bat" file. The file named "halite.exe" is your new environment binary.</p>
            </div>
        </div>
        <?php include 'includes/footer.php'; ?>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
