<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>Game Rules</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
			<div class="col-sm-12">
        <h1>Applying Machine Learning to Halite</h1>

        <h3>Overview</h3>
        <p>We are going to show you a simple example of how machine learning applied to Halite. We will locally train a neural network to mimic Matt Aderth's current halite bot (as of July 13th) using the <a>Keras library</a>.</p>

        <h3>Data Aquisition</h3>
	      <p>
					Here is an archive of a couple thosand games that Matt's bot plays in. We can load and parse these files into lists of `GameMap` objects with this code:
					<pre>
						<code>

						</code>
					</pre>
				</p>

				<p>
					We need to take the games that we have loaded and transform them into data that we can use to train our neural network. How might we do that? What data do we want our neural network to consider before moving a piece? What is the neural network outputting? To keep this tutorial simple, our neural network will only be able to "see" the 3 by 3 grid surrounding the piece that it has to move and will output the direction that it wants to move a piece.
				</p>

				<p>
					We may transform our loaded games into the information that we care about, the 3 by 3 bounding box around a piece and the direction Matt chose to move that piece, with the following code:

					<pre>
						<code>

						</code>
					</pre>
				</p>

				<p>
					We can further transform this information into inputs and ouptuts that the neural network can accept using this code:

					<pre>
						<code>

						</code>
					</pre>
				</p>

        <h3>Training Our Model</h3>
				<p>
					Now that we have the necessary training data, we need to build and train our neural network.
				</p>

				<p>
					Our neural network has 9 inputs. We will restrict our neural network to one hidden layer of 18 neurons with sigmoid activation function. The neural network has five outputs, so five output layer neurons, each corresponding to one of the five directions that the bot may move in (north, south, east, west, and still). We will apply the Softmax activation function to the output layer.

					<pre>
						<code>

						</code>
					</pre>
				</p>

				<p>
					Training our bot is trivial in keras.
					<pre>
						<code>

						</code>
					</pre>
				</p>

				<p>
					Once training has finished, we want to store our model so that we can use it to select moves.
					<pre>
						<code>

						</code>
					</pre>
				</p>

				<h3>Running Our Bot</h3>
				<p>
					Our actual bot's source needs to load our trained neural network and use it to select moves for all of our pieces, every turn.
				</p>

				<p>
					Here is our complete bot source:
					<pre>
						<code>

						</code>
					</pre>
				</p>

				<p>
					We are now ready to submit our bot to the server! Though it's not perfect at mimicing Matt's bot, since he considers the entire board instead of just a 3 by 3 slice of it when moving a piece, it does beat a Basic Bot.
				</p>

        <h3>Other Libraries</h3>
        <p>
          In addition to keras, the following libraries are available on our servers:
          <ul>
            <li>Numpy 1.11.1</li>
            <li>Tensorflow 0.9.0</li>
            <li>Theano 0.8.2</li>
          </ul>
        </p>
			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
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
