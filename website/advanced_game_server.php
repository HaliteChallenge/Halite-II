<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Game Servers</title>

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

                <h1>Game Servers</h1>

                <h3>Hardware</h3>
                <p>All compilation and game execution is done on AWS EC2 m3.medium servers running Ubuntu 16.04. They have the following specs:
                    <ul>
                        <li>3.5 GB of RAM</li>
                        <li>~10 GB of disk space</li>
                        <li>1 CPU</li>
                    </ul>
                </p>

                <h3>Sandbox</h3>
                <p>During compilation and games, players are run as unprivileged users in a sandbox which limits memory, processing power, execution time, disk space, and internet access. The sandbox is a docker container defined by this <a href="https://github.com/HaliteChallenge/Halite/blob/master/worker/Dockerfile">Dockerfile</a>, running on top of Docker 1.6.2. If you think that there is an issue with the sandbox, please email us at <a href="mailto:halite@halite.io">halite@halite.io</a></b>.

                    <h3>Compilation</h3>
                    <p>Bot compilation is done using <a href="https://github.com/HaliteChallenge/Halite/blob/master/worker/compiler.py">this autocompile script</a>.</p>
                    <p>To facilitate the installation of custom software, we allow users to include an install script. If a file named <code>install.sh</code> exists in your submission, it is run as a bash script under the root user in a sandbox with internet access and 10 minutes of runtime. Bots may only read and write to their current directory, so all files that you want to be available at runtime must be installed locally. For more on using 3rd party libraries, click <a href="advanced_libraries.php">here</a>.</p>
                    <p>
                        Your main file must be called <code>MyBot</code>. Your language is recognized using the file extension of your <code>MyBot</code> file. The appropriate file extensions for each language are:
                        <ul>
                            <li>Java - .java</li>
                            <li>Python - .py</li>
                            <li>C++ - .cpp and .h(pp)</li>
                            <li>C# - .cs</li>
                            <li>Rust - .toml (for your Cargo.toml) and .rs (for your Rust source)</li>
                            <li>Scala - .scala</li>
                            <li>Ruby - .rb</li>
                            <li>Go - .go</li>
                            <li>PHP - .php</li>
                            <li>JavaScript - .js</li>
                            <li>OCaml - .ml</li>
                            <li>Clojure - .clj</li>
                            <li>C - .c</li>
                            <li>Julia - .jl</li>
                        </ul>
                    </p>

                    <p>
                        The following compilers are used:
                        <ul>
                            <li>Java - javac 1.8.0_111</li>
                            <li>C++ - g++ 4.8</li>
                            <li>C# - mcs 4.6.1.0</li>
                            <li>Rust - rustc 1.10.0</li>
                            <li>Scala - scalac 2.10.4</li>
                            <li>C - gcc 4.8</li>
                        </ul>
                    </p>

                    <p>
                        The following build automators are used:
                        <ul>
                            <li>Rust - cargo 0.11.0</li>
                            <li>Clojure - lein 2.7.0</li>
                        </ul>
                    </p>

                    <p>
                        The following versions of each language are supported:
                        <ul>
                            <li>Java 8</li>
                            <li>Python 3.4.3</li>
                            <li>C++ 11</li>
                            <li>C# 6.0</li>
                            <li>Rust 1.10</li>
                            <li>Scala 2.10.4</li>
                            <li>Ruby 2.3.1</li>
                            <li>Go 1.6</li>
                            <li>PHP 7.0</li>
                            <li>Node.js (JavaScript) 7.1.0</li>
                            <li>OCaml 4.01.0</li>
                            <li>Clojure 1.8.0</li>
                            <li>Julia 0.5.0</li>
                        </ul>
                    </p>

                    <h3>Games</h3>
                    <p>Bots are given <b>250 MB of RAM</b> and <b>equal amounts of CPU</b>.</p>
                    <p>Currently, 20x20, 25x25, 30x30, 35x35, 40x40, 45x45, and 50x50 games are run. Games may be 2-6 player.</p>
                    <p>Games are always run using <a href="downloads.php">the most recent environment build</a>.</p>
                </p>
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
