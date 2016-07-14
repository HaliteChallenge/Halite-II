<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>BFS Tutorial</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
		<div class="col-sm-12">

			<h1>Breadth-First Search Tutorial</h1>

			<p>The source code for this tutorial is located <a href="https://github.com/HaliteChallenge/Halite/blob/master/website/tutorials/bfs/">here</a>.</p>

      <h3>Introduction</h3>

      <p>
				The Basic Bot we wrote in the previous tutorial was certainly a big step up on the random bot provided in the starter packages, but we can do much better. The central issue with the Basic Bot is that although it utilizes pieces on its borders, pieces that are not on borders just move randomly once they have a fairly high strength. This is hugely inefficient for a few reasons:
        <ul>
        	<li>The bot will often accidentally move pieces whose strengths some to greater than 255 into each other. Due to the strength cap, this lowers the overall strength of the bot.</li>
        	<li>The bot doesn't effectively utilize its strength to expand or attack its opponents. It simply relies on a piece coming into a border by random chance, and when a piece is more than a few squares away from a border this very rarely happens. These pieces are not appreciably helping the bot to win the game.</li>
        	<li>The bot doesn't move pieces still as often as it could. To minimize overflow waste from the 255 cap, the bot limits how long it will have pieces remain still before it begins to move them randomly. An algorithm better at moving strength to the borders of a bot would allow the bot to make more still move, which translates to a higher production efficiency and more overall strength produced by the bot.</li>
        </ul>
			</p>

      <p>There are many ways to solve these problems, and the one we'll be briefly going through is the use of the breadth-first search in efficiently routing pieces to the edges.</p>

      <h3>Algorithm</h3>

      <p>Our algorithm will consist of a queue of locations. We'll initialize the queue to contain all of the pieces we don't own. Whenever we pop off of the queue, we'll add on all the pieces adjacent to it which haven't been visited yet and point those locations towards the location which we took off of the queue. Finally, we'll go through the game map and move the pieces we want to move those in the directions we found using the breadth-first search.</p>

      <p>Let's see what this bot will look like in Java!</p>

      <p>First, we have the same declarations and objects as we had in the BasicBot.</p>
      <pre><code>InitPackage iPackage = Networking.getInit();
int myID = iPackage.myID;
GameMap gameMap = iPackage.map;

Networking.sendInit("BfsBot");

while(true) {
	ArrayList&lt;Move> moves = new ArrayList&lt;Move>();
	gameMap = Networking.getFrame();
	...
			</code></pre>

		<p>Next we'll add the structures we'll need for our breadth-first search.</p>
		<p>
			This is our map of which squares we've visited. Whenever we add another location to the queue, we'll mark it as visited here so we don't add squares multiple times.
			<pre><code>ArrayList&lt; ArrayList&lt;Boolean> > visited = new ArrayList&lt; ArrayList&lt;Boolean> >();
for(int y = 0; y < gameMap.height; y++) {
	ArrayList&lt;Boolean> vRow = new ArrayList&lt;Boolean>();
	for(int x = 0; x < gameMap.width; x++) {
		vRow.add(false);
	}
	visited.add(vRow);
}</code></pre>
			</p>

			<p>
				Here we initialize our map of directions. Whenever we add a location to the queue, we'll set the direction here to be the one that points towards the location we popped off of the queue that it was adjacent to.
				<pre><code>ArrayList&lt; ArrayList&lt;Direction> > directions = new ArrayList&lt;ArrayList&lt;Direction> >();
for(int y = 0; y < gameMap.height; y++) {
	ArrayList&lt;Direction> dRow = new ArrayList&lt;Direction>();
	for(int x = 0; x < gameMap.width; x++) {
		dRow.add(Direction.STILL);
	}
	directions.add(dRow);
}</code></pre>
			</p>

			<p>
				Now we can add the queue for our search! We'll initialize it with the locations that we don't own, as it's those that we're trying to reach.
			<pre><code>LinkedList&lt;Location> toVisit = new LinkedList&lt;Location>(); // LinkedList just happens to be a structure which implements queue; there are others that would work as well.
for(int y = 0; y < gameMap.height; y++) {
	for(int x = 0; x < gameMap.width; x++) {
		Location l = new Location(x, y);
		Site site = gameMap.getSite(l);
		if(site.owner != myID) {
			toVisit.add(l);
			visited.get(y).set(x, true);
		}
	}
}</code></pre>

		</p>

		<p>Next we'll need to actually use this to direct our pieces. So, we'll continually pop off of the front of the queue, add the adjacent unvisited pieces, mark their directions and them as visited, and ensure that the queue isn't empty.
		<pre><code>while(!toVisit.isEmpty()) {
	Location l = toVisit.remove();
	visited.get(l.y).set(l.x, true);
	Location n = gameMap.getLocation(l, Direction.NORTH), e = gameMap.getLocation(l, Direction.EAST), s = gameMap.getLocation(l, Direction.SOUTH), w = gameMap.getLocation(l, Direction.WEST);
	if(!visited.get(n.y).get(n.x)) {
		toVisit.add(n);
		visited.get(n.y).set(n.x, true);
		directions.get(n.y).set(n.x, Direction.SOUTH);
	}
	if(!visited.get(e.y).get(e.x)) {
		toVisit.add(e);
		visited.get(e.y).set(e.x, true);
		directions.get(e.y).set(e.x, Direction.WEST);
	}
	if(!visited.get(s.y).get(s.x)) {
		toVisit.add(s);
		visited.get(s.y).set(s.x, true);
		directions.get(s.y).set(s.x, Direction.NORTH);
	}
	if(!visited.get(w.y).get(w.x)) {
		toVisit.add(w);
		visited.get(w.y).set(w.x, true);
		directions.get(w.y).set(w.x, Direction.EAST);
	}
}</code></pre></p>

		<p>Next, we'll go through the map. If a piece's strength is too low, we won't move it; else we'll move it as given to by our directions map.
		<pre><code>for(int y = 0; y < gameMap.height; y++) {
	for(int x = 0; x < gameMap.width; x++) {
		Site site = gameMap.getSite(new Location(x, y));
		if(site.owner == myID) {
			if(site.strength > 5 * site.production || site.strength == 255) moves.add(new Move(new Location(x, y), directions.get(y).get(x)));
			else moves.add(new Move(new Location(x, y), Direction.STILL));
		}
	}
}</code></pre></p>

		<p>Finally, we'll send our moves.
		<pre><code>Networking.sendFrame(moves);</code></pre>
		</p>

	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
          <li class="previous"><a href="basic.php"><span aria-hidden="true">&larr;</span> Basic Bot Tutorial</a></li>
					<li class="next"><a href="machine_learning_tutorial.php">Machine Learning Tutorial <span aria-hidden="true">&rarr;</span> </a></li>
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
