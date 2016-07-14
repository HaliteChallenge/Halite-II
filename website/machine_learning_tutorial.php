<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>Machine Learning Tutorial</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
			<div class="col-sm-12">
        <h1>Applying Machine Learning to Halite</h1>

				<p>The source code for this tutorial is located <a href="https://github.com/HaliteChallenge/Halite/blob/master/website/tutorials/machinelearning/">here</a>.</p>

        <h3>Overview</h3>
        <p>We are going to show you a simple example of how machine learning applied to Halite. We will locally train a neural network to mimic Matt Aderth's current halite bot (as of July 13th) using the <a href="http://keras.io">Keras library</a>.</p>

				<h3>Installation</h3>
				<p>
					The run the code included in this tutorial, you will need to download the keras and h5py libraries. This can be done on <b>Debian</b> like so:
					<pre><code>apt-get install -y python3-numpy python3-scipy python3-dev python3-pip python3-nose g++ libblas-dev git
pip3 install Theano

git clone https://github.com/fchollet/keras.git
cd keras
python3 setup.py install

apt-get install -y python3-h5py</code></pre>
				</p>

        <h3>Data Aquisition</h3>
	      <p>
					<a href="tutorials/Replays.zip">Here</a> is an archive of about 500 games that Matt's bot participates in. We want load and parse these files into lists of `GameMap` objects and moves, so that we can use the data contained in them. We can load in the data we need from an HLT file with this code:
					<pre><code>def loadGame(filename):
	def bytesUntil(gameFile, endByte):
	    byteArray = []
	    byte = gameFile.read(1)
	    while byte != endByte:
	        byteArray.append(byte)
	        byte = gameFile.read(1)
	    return byteArray

	def stringUntil(gameFile, endChar):
	    returnString = ""
	    byte = gameFile.read(1)
	    while byte != endChar.encode("utf-8"):
	        returnString += byte.decode("utf-8")
	        byte = gameFile.read(1)
	    return returnString
	mattID = None
	frames = []
	moves = []

	gameFile = open(filename, "rb")
	try:
	    stringUntil(gameFile, "\n")

	    # Get metadata
	    metadata = stringUntil(gameFile, "\n")

	    components = metadata.split(" ")
	    width = int(components.pop(0))
	    height = int(components.pop(0))
	    numPlayers = int(components.pop(0))
	    numFrames = int(components.pop(0))

	    # Get matt's playerID
	    for playerID in range(1, numPlayers+1):
	        name = stringUntil(gameFile, "\0")
	        if name == "adereth":
	            mattID = playerID
	        stringUntil(gameFile, "\n")

	    # Get production
	    productions = [int.from_bytes(gameFile.read(1), byteorder='big') for a in range(width*height)]
	    gameFile.read(1)

	    # Get the frames and moves
	    for frameIndex in range(numFrames-1):
	        # Frames
	        frames.append(GameMap(width=width, height=height, numberOfPlayers=numPlayers))
	        x = 0
	        y = 0
	        while y &lt; height:
	            numTiles = int.from_bytes(gameFile.read(1), byteorder='big')
	            ownerID = int.from_bytes(gameFile.read(1), byteorder='big')

	            strengths = []
	            for a in range(numTiles):
	                frames[-1].contents[y][x] = Site(ownerID, int.from_bytes(gameFile.read(1), byteorder='big'), productions[y*width + x])

	                x += 1
	                if x == width:
	                    x = 0
	                    y += 1
	                    if y == height:
	                        break
	        # Moves
	        moves.append({(index % width, math.floor(index/width)):int.from_bytes(gameFile.read(1), byteorder='big') for index in range(width*height)})
	finally:
	    gameFile.close()
	return mattID, frames, moves</code></pre>
				</p>

				<p>
					Now, we need to take the games that we have loaded and transform them into data that we can use to train our neural network. How might we do that? What data do we want our neural network to consider before moving a piece? What data do we want the neural network to output? To keep this tutorial simple, our neural network will only be able to "see" the 3 by 3 grid surrounding the piece that it has to move and will output the direction that it wants to move a piece.
				</p>

				<p>
					Great, but what will the actual inputs and outputs of the neural network? How are we going to feed information about the 3 by 3 grid surrounding a piece to the model? Our input scheme will look like this:
					<code>isEnemy<sub>1</sub> normalizedStrength<sub>1</sub> normalizedProduction<sub>1</sub> ... isEnemy<sub>8</sub> normalizedStrength<sub>8</sub> normalizedProduction<sub>8</sub></code>
				</p>

				<p>
					Our output scheme will look like this:
					<code>isStill isNorth isEast isSouth isWest</code>
			</p>

				<p>
					Now lets take all of our games (assumed to be in a folder called replays) and transform them to the input and ouptut schemes that we specified above:

					<pre><code>def getNNData():
	inputs = []
	correctOutputs = []

	gamePath = "replays"

	for filename in [f for f in listdir(gamePath) if isfile(join(gamePath, f))]:
		print("Loading " + filename)

		mattID, frames, moves = loadGame(join(gamePath, filename))
		maxProduction = 0
		for y in range(frames[0].height):
			for x in range(frames[0].width):
				prod = frames[0].getSite(Location(x, y)).production
				if prod > maxProduction:
					maxProduction = prod
		for turnIndex in range(len(moves)):
			gameMap = frames[turnIndex]
			for y in range(gameMap.height):
				for x in range(gameMap.width):
					loc = Location(x, y)
					if gameMap.getSite(loc).owner == mattID:
						box = [gameMap.getSite(gameMap.getLocation(loc, NORTH), WEST), gameMap.getSite(loc, NORTH), gameMap.getSite(gameMap.getLocation(loc, NORTH), EAST), gameMap.getSite(loc, EAST), gameMap.getSite(gameMap.getLocation(loc, SOUTH), EAST), gameMap.getSite(loc, SOUTH), gameMap.getSite(gameMap.getLocation(loc, SOUTH), WEST), gameMap.getSite(loc, WEST)]
						nnInput = []
						for site in box:
							nnInput += [1 if site.owner == mattID else -1, float(site.strength / 255), float(site.production / maxProduction)]
						inputs.append(nnInput)
						correctOutputs.append([1 if a == moves[turnIndex][(x, y)] else 0 for a in range(5)])
	return inputs, correctOutputs</code></pre>
				</p>


        <h3>Training the Model</h3>
				<p>
					Now that we have the necessary training data, we need to build and train our neural network.
				</p>

				<p>
					Our neural network will use the hyperbolic tangent activation function for its input and hidden layers. We will restrict it to just one hidden layer. Its output layer will use the softmax activation function. We will use stochastic gradient descent as our training algorithm.

					<pre><code>inputs, correctOutputs = getNNData()

print("Collected data")

trainingInputs = inputs[:len(inputs)//2]
trainingOutputs = correctOutputs[:len(correctOutputs)//2]

testInputs = inputs[len(inputs)//2:]
testOutputs = correctOutputs[len(correctOutputs)//2:]

model = Sequential()
model.add(Dense(24, input_shape=(24, )))
model.add(Activation('tanh'))
model.add(Dense(24))
model.add(Activation('tanh'))
model.add(Dense(5))
model.add(Activation('softmax'))

model.summary()

model.compile(loss='mean_squared_error', optimizer=SGD(lr=0.1, decay=1e-6, momentum=0.9, nesterov=True))

model.fit(trainingInputs, trainingOutputs, validation_data=(testInputs, testOutputs))
score = model.evaluate(testInputs, testOutputs, verbose=0)
print("Loss:" + str(score))</code></pre>
				</p>

				<p>
					Once training has finished, we want to store our model so that we can use it to select moves.
					<pre><code>json_string = model.to_json()
open('my_model_architecture.json', 'w').write(json_string)
model.save_weights('my_model_weights.h5')</code></pre>
				</p>

				<p>You should now be able to run the training script. The full copy of it is located <a href="https://github.com/HaliteChallenge/Halite/blob/master/website/tutorials/machinelearning/TrainMatt.py">here</a>. You should expect output similar to this:
					<pre><code>Using Theano backend.
Loading 1468472193187540.hlt
Loading 1468471868771943.hlt
Loading 1468462788126287.hlt
Loading 1468472089094339.hlt
Loading 1468467773230327.hlt
Loading 1468459408179415.hlt
Collected data
____________________________________________________________________________________________________
Layer (type)                     Output Shape          Param #     Connected to
====================================================================================================
dense_1 (Dense)                  (None, 24)            600         dense_input_1[0][0]
____________________________________________________________________________________________________
activation_1 (Activation)        (None, 24)            0           dense_1[0][0]
____________________________________________________________________________________________________
dense_2 (Dense)                  (None, 24)            600         activation_1[0][0]
____________________________________________________________________________________________________
activation_2 (Activation)        (None, 24)            0           dense_2[0][0]
____________________________________________________________________________________________________
dense_3 (Dense)                  (None, 5)             125         activation_2[0][0]
____________________________________________________________________________________________________
activation_3 (Activation)        (None, 5)             0           dense_3[0][0]
====================================================================================================
Total params: 1325
____________________________________________________________________________________________________
Train on 54556 samples, validate on 54556 samples
Epoch 1/10
54556/54556 [==============================] - 3s - loss: 0.0856 - val_loss: 0.0828
Epoch 2/10
54556/54556 [==============================] - 3s - loss: 0.0827 - val_loss: 0.0824
Epoch 3/10
54556/54556 [==============================] - 3s - loss: 0.0821 - val_loss: 0.0836
Epoch 4/10
54556/54556 [==============================] - 3s - loss: 0.0815 - val_loss: 0.0821
Epoch 5/10
54556/54556 [==============================] - 3s - loss: 0.0811 - val_loss: 0.0833
Epoch 6/10
54556/54556 [==============================] - 2s - loss: 0.0807 - val_loss: 0.0825
Epoch 7/10
54556/54556 [==============================] - 0s - loss: 0.0802 - val_loss: 0.0812
Epoch 8/10
54556/54556 [==============================] - 0s - loss: 0.0799 - val_loss: 0.0826
Epoch 9/10
54556/54556 [==============================] - 0s - loss: 0.0796 - val_loss: 0.0821
Epoch 10/10
54556/54556 [==============================] - 0s - loss: 0.0793 - val_loss: 0.0814
Loss: 0.0814138701757</code></pre>
				</p>

				<p>If you do not want to wait for your bot to be trained, you can grab the <code>json</code> and <code>hd5</code> files for a model trained on ~50 games <a href="https://github.com/HaliteChallenge/Halite/tree/master/website/tutorials/machinelearning">here</a>.
				</p>

				<h3>Running the Model</h3>
				<p>
					Now that our model has been trained and saved, our actual bot's source needs to load our trained neural network and use it to select moves for all of our pieces, every turn.
				</p>

				<p>
					Here is our complete bot source:
					<pre><code>from hlt import *
from networking import *

from keras.models import Sequential, model_from_json
from keras.layers import Dense, Activation
from keras.optimizers import SGD, Adam, RMSprop

import numpy as np

myID, gameMap = getInit()

model = model_from_json(open('my_model_architecture.json').read())
model.load_weights('my_model_weights.h5')
model.compile(loss='mean_squared_error', optimizer=SGD(lr=0.1, decay=1e-6, momentum=0.9, nesterov=True))


maxProduction = 0
for y in range(gameMap.height):
	for x in range(gameMap.width):
		prod = gameMap.getSite(Location(x, y)).production
		if prod > maxProduction:
			maxProduction = prod

sendInit("MattCopy")

while True:
	moves = []
	gameMap = getFrame()
	for y in range(gameMap.height):
		for x in range(gameMap.width):
			loc = Location(x, y)
			if gameMap.getSite(loc).owner == myID:
				box = [gameMap.getSite(gameMap.getLocation(loc, NORTH), WEST), gameMap.getSite(loc, NORTH), gameMap.getSite(gameMap.getLocation(loc, NORTH), EAST), gameMap.getSite(loc, EAST), gameMap.getSite(gameMap.getLocation(loc, SOUTH), EAST), gameMap.getSite(loc, SOUTH), gameMap.getSite(gameMap.getLocation(loc, SOUTH), WEST), gameMap.getSite(loc, WEST)]
				nnInput = []
				for site in box:
					nnInput += [1 if site.owner == myID else -1, float(site.strength / 255), float(site.production / maxProduction)]
				nnInput = np.asarray(nnInput).reshape((1, 24))

				output = model.predict(nnInput)[0]

				biggest = -222
				direction = STILL
				for d in range(len(output)):
					if output[d] > biggest:
						biggest = output[d]
						direction = d
				moves.append(Move(loc, direction))
	sendFrame(moves)</code></pre>
				</p>

				<p>
					We are now ready to submit our bot to the server! Though it's not perfect at mimicing Matt's bot, since he considers the entire board instead of just a 3 by 3 slice of it when moving a piece, it does beat a Basic Bot. We hope that this tutorial helped to start you thinking about how to apply machine learning to Halite!
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
					<li class="previous"><a href="bfs_tutorial.php"><span aria-hidden="true">&larr;</span> BFS Tutorial </a></li>
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
