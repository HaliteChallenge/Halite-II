<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>Build Environment</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
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
            <li>512 MB of RAM</li>
            <li>20 GB of disk spaces</li>
            <li>512 MB of RAM</li>
            <li>1 processor core</li>
          </ul>
        </p>

				<h3>Libraries</h3>
				<p>
					The following libraries will available at compiletime and runtime by our launch on Thursday:
					<ul>
						<li>Tensorflow</li>
						<li>Numpy</li>
					</ul>

					If you would like to use a library that is not listed above, post on the forums. If a library is trivial to build, feel free to include its source with your bot submission.
				</p>

        <h3>Compilation</h3>
        <p>Bot compilation is done using <a href="https://github.com/HaliteChallenge/Halite/blob/master/worker/compiler.py">this autocompile script</a>.</p>

        <p>
          Your main file must be called <code>MyBot</code>. Your language is recognized using the file extension of your <code>MyBot</code> file. The appropriate file extensions for each language are:
          <ul>
            <li>Java - .java</li>
            <li>Python - .py</li>
            <li>C++ - .cpp and .h</li>
          </ul>
        </p>

        <p>
          The following compilers are used:
          <ul>
            <li>Java - javac 1.7.0_101</li>
            <li>C++ - g++ 4.84</li>
          </ul>
        </p>

        <p>
          The following versions of each language are supported:
          <ul>
            <li>Java 7+</li>
            <li>Python 3.4.3</li>
            <li>C++ 11</li>
          </ul>
        </p>

        <h3>Games</h3>
        <p>Games are always run using <a href="downloads.php">the most recent environment build</a>.</p>

				<p>Players interface with the environment from within a sandbox which limits memory, threads, execution time, and internet access. The sandbox is a docker container defined by <a href="https://github.com/HaliteChallenge/Halite/blob/master/worker/Dockerfile">this Dockerfile</a> and running on top of Docker 1.6.2. <a href="https://github.com/remoteinterview/compilebox">A version of the sandbox</a> is used by the coding interview company <a href="https://www.remoteinterview.io/">remoteinterview</a> to secure their web ide. <b>If you think that there is an issue with the sandbox, please email us at halite@halite.io</b>.</p>
			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
          <li class="previous"><a href="networking_spec.php"><span aria-hidden="true">&larr;</span> Game Spec</a></li>
					<li class="next"><a href="networking_spec.php">Networking Spec <span aria-hidden="true">&rarr;</span> </a></li>
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
