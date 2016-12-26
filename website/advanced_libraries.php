<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Submitting A Bot</title>

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
                <h1>Submitting A Bot</h1>

                <p>You submit a zip file containing your <code>MyBot</code> file and any other files needed to compile and run your <code>MyBot</code> file.</p>

                <h3>Installing Dependencies</h3>

                <p>Instead of packaging up your dependencies inside your zip file, you may include a bash file named <code>install.sh</code> that will be run before your bot is compiled.</p>

                <p>It will be run with internet access and write access to only its current directory. It may run for a maximum of 10 minutes and will not be able to make global installation changes (i.e. <code>apt-get</code> will not work).</p>

                <h4>Package Managers</h4>

                <p>The following package managers are already installed on the server and can be used to install dependencies locally:</p>

                <ul>
                <li><code>pip3</code></li>
                <li><code>bundler</code></li>
                <li><code>npm</code></li>
                </ul>

                <p><code>curl</code> is also available and can be used to download additional runtimes, tools, and environments.</p>

                <p>If your library isn't on a package manager that supports local installation and you can’t download it with <code>curl</code>, you are going to have to compile it on our game servers. Include the source of you library in your bot's zip file and put compilation instructions in the <code>install.sh</code> file.</p>

                <h4>Preinstalled Libraries</h4>
                <p>For convenience's sake, we include <code>tensorflow</code>, <code>keras</code> (using a <code>tensorflow</code> backend), <code>numpy</code>, <code>scipy</code>, <code>scikit-learn</code>, <code>pillow</code>, and <code>h5py</code> on our game servers. Just import these libraries from your python files like normal!</p>

                <h3>Compilation</h3>

                <p>Bot compilation is done using <a href="https://github.com/HaliteChallenge/Halite/blob/master/worker/compiler.py">this autocompile script</a>. Many languages will be properly autodetected and compiled if needed without the need for an <code>install.sh</code> script.</p>

                <p>Your main file must be called <code>MyBot</code>. Your language is recognized using the file extension of your <code>MyBot</code> file. The appropriate file extensions for each language are:</p>

                <ul>
                <li>Java - <code>.java</code></li>
                <li>Python - <code>.py</code></li>
                <li>C++ - <code>.cpp</code> and <code>.h(pp)</code></li>
                <li>C# - <code>.cs</code></li>
                <li>Rust - <code>.toml</code> (for your <code>Cargo.toml</code>) and <code>.rs</code> (for your Rust source)</li>
                <li>Scala - <code>.scala</code></li>
                <li>Ruby - <code>.rb</code></li>
                <li>Go - <code>.go</code></li>
                <li>PHP - <code>.php</code></li>
                <li>JavaScript - <code>.js</code></li>
                <li>OCaml - <code>.ml</code></li>
                <li>Clojure - <code>.clj</code></li>
                <li>C - <code>.c</code></li>
                </ul>

                <p>See the <a href="advanced_game_server.php">Game Server Reference</a> for details about compiler and runtime versions.</p>

                <h4>Customizing your language name</h4>

                <p>If you are using a language that is generic or that does not have first class support on the server, you can include a file named <code>LANGUAGE</code> containing the name of the language you are using. This will be used only for display on the rankings and in your profile.</p>

                <h4>JVM Languages</h4>

                <p>For JVM languages, you can submit a jar file inside of your zip file instead of source files. The jar will be executed <code>java -jar MyBot.jar</code> so you need to <a href="https://docs.oracle.com/javase/tutorial/deployment/jar/appman.html">define a Main-Class header in the manifest</a>.</p>

                <h3>Running</h3>

                <p>You may supply a <code>run.sh</code> script to control how your bot is run. Many languages will be properly autodetected and run without the need for an <code>install.sh</code> script. You should only include a custom <code>run.sh</code> script if you have a real need for one.</p>

                <h4>Custom Runtime</h4>

                <p>You could use a <code>run.sh</code> file to use a custom runtime such as PyPy instead of the default Python 3.</p>

                <h3>Understanding Game Logs</h3>

                <p>When your bot times out or errors on our game servers, we save and display a log file with debugging information including the time your bot took each turn, its output each turn, and its final output from stdout and stderr.</p>

                <p>To find these log files, visit your <a href="user.php">homepage</a>. Just click the download log button to grab your error log for a game.</p>

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
