<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Build Environment</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
    <link href="style/learn.css" rel="stylesheet">
</head>
<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="row">
            <div class="col-sm-12">

                <h1>Build Environment Specification</h1>

                <h3>Hardware</h3>
                <p>All compilation and game execution is done on Ubuntu 14.04 servers hosted by Digital Ocean with the following specs:
                    <ul>
                        <li>8 GB of RAM</li>
                        <li>80 GB of disk space</li>
                        <li>4 processor cores</li>
                    </ul>
                </p>

                <h3>Sandbox</h3>
                <p>During compilation and games, players are contained in a sandbox which limits memory, processing power, execution time, and internet access. The sandbox is a docker container defined by <a href="https://github.com/HaliteChallenge/Halite/blob/master/worker/Dockerfile">this Dockerfile</a>, running on top of Docker 1.6.2. <a href="https://github.com/remoteinterview/compilebox">A version of the sandbox</a> is used by the coding interview company <a href="https://www.remoteinterview.io/">remoteinterview</a> to secure their web ide. <b>If you think that there is an issue with the sandbox, please email us at halite@halite.io</b>.

                    <h3>Compilation</h3>
                    <p>Bot compilation is done using <a href="https://github.com/HaliteChallenge/Halite/blob/master/worker/compiler.py">this autocompile script</a>.</p>
                    <p>
                        To facilitate the installation of custom software, we allow users to include an install script. If a file named <code>install.sh</code> exists in your submission, it is run as a bash script under the root user in a sandbox with internet access and 10 minutes of runtime. Any files that you want to be available at runtime must be installed locally.
                    </p>
                    <p>For example, if your bot required the trueskill python library, you would only need to include the command: <code>pip3 install trueskill -t ./</code> in your <code>install.sh</code> file to have it available at runtime.</p>
                    <p>
                        The following libraries are already available at compiletime and runtime:
                        <ul id="available-libraries">
                            <li>Numpy 1.11.1</li>
                            <li>h5py 2.2.1</li>
                            <li>Tensorflow 0.9.0</li>
                            <li>Theano 0.8.2</li>
                            <li>Keras 1.0.5 (using Theano as a backend)</li>
                            <li>jsonpickle 0.9.3</li>
                            <li>pandas 0.18.1</li>
                        </ul>
                    </p>
                    <p>
                        Your main file must be called <code>MyBot</code>. Your language is recognized using the file extension of your <code>MyBot</code> file. The appropriate file extensions for each language are:
                        <ul>
                            <li>Java - .java</li>
                            <li>Python - .py</li>
                            <li>C++ - .cpp and .h</li>
                            <li>Rust - .toml (for your Cargo.toml) and .rs (for your Rust source)</li>
                            <li>Scala - .scala</li>
                        </ul>
                    </p>

                    <p>
                        The following compilers are used:
                        <ul>
                            <li>Java - javac 1.7.0_101</li>
                            <li>C++ - g++ 4.84</li>
                            <li>Rust - rustc 1.10.0</li>
                            <li>Scala - scalac 2.10.4</li>
                        </ul>
                    </p>

                    <p>
                        The following build automators are used:
                        <ul>
                            <li>Rust - cargo 0.11.0</li>
                        </ul>
                    </p>

                    <p>
                        The following versions of each language are supported:
                        <ul>
                            <li>Java 7</li>
                            <li>Python 3.4.3</li>
                            <li>C++ 11</li>
                            <li>Rust 1.10</li>
                            <li>Scala 2.10.4</li>
                        </ul>
                    </p>

                    <h3>Games</h3>
                    <p>Bots are given <b>512 MB of RAM</b> and <b>equal amounts of CPU</b>.</p>
                    <p>Currently, 20x20, 25x25, 30x30, 35x35, 40x40, 45x45, and 50x50 games are run. Games may be 2-6 player.</p>
                    <p>Games are always run using <a href="downloads.php">the most recent environment build</a>.</p>
                </p>
            </div>
        </div>
    </div>

    <footer class="footer pageContent">
        <div class="container">
            <div id="footer">
                <ul class="pager">
                    <li class="previous"><a href="io_spec.php"><span aria-hidden="true">&larr;</span> IO Spec</a></li>
                    <li class="next"><a href="contest_spec.php">Contest Spec <span aria-hidden="true">&rarr;</span> </a></li>
                </ul>
            </div>
        </div>
    </footer>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
