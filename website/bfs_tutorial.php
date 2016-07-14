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

		<h1>Tutorial: Breadth-First Search</h1>

        <h3>Introduction</h3>

        <p>The Basic Bot we wrote in the previous tutorial was certainly a big step up on the random bot provided in the starter packages, but we can do much better. The central issue with the Basic Bot is that although it utilizes pieces on its borders, pieces that are not on borders just move randomly once they have a fairly high strength. This is hugely inefficient for a few reasons:
        <ul>
        	<li>The bot will often accidentally move pieces whose strengths some to greater than 255 into each other. Due to the strength cap, this lowers the overall strength of the bot.</li>
        	<li>The bot doesn't effectively utilize its strength to expand or attack its opponents. It simply relies on a piece coming into a border by random chance, and when a piece is more than a few squares away from a border this very rarely happens. These pieces are not appreciably helping the bot to win the game.</li>
        	<li>The bot doesn't move pieces still as often as it could. To minimize overflow waste from the 255 cap, the bot limits how long it will have pieces remain still before it begins to move them randomly. An algorithm better at moving strength to the borders of a bot would allow the bot to make more still move, which translates to a higher production efficiency and more overall strength produced by the bot.</li>
        </ul></p>

        <p>There are many ways to solve these problems, and the one we'll be briefly going through is the use of the breadth-first search in efficiently routing pieces to the edges.</p>

        <h3>Algorithm</h3>

        <p>Our algorithm will consist of a queue of locations. We'll initialize the queue to contain all of the pieces we don't own. Whenever we pop off of the queue, we'll add on all the pieces adjacent to it which haven't been visited yet and point those locations towards the location which we took off of the queue. Finally, we'll go through the game map and move the pieces we want to move those in the directions we found using the breadth-first search.</p>

        <p>Let's see what this bot will look like in Java!</p>

        <p>First, we have the same declarations and objects as we had in the BasicBot.</p>
        <code>InitPackage iPackage = Networking.getInit();<br>
	    int myID = iPackage.myID;<br>
	    GameMap gameMap = iPackage.map;<br>
	    <br>
	    Networking.sendInit("BfsBot");<br>

	    while(true) {<br>
			ArrayList<Move> moves = new ArrayList<Move>();<br>
			gameMap = Networking.getFrame();<br>
		</code>

		<p>Next we'll add the structures we'll need for our breadth-first search.
		<ul>
			<li>This is our map of which squares we've visited. Whenever we add another location to the queue, we'll mark it as visited here so we don't add squares multiple times.<br>
			<code>ArrayList< ArrayList<Boolean> > visited = new ArrayList< ArrayList<Boolean> >();<br>
			for(int y = 0; y < gameMap.height; y++) {<br>
				ArrayList<Boolean> vRow = new ArrayList<Boolean>();<br>
				for(int x = 0; x < gameMap.width; x++) {<br>
					vRow.add(false);<br>
				}<br>
				visited.add(vRow);<br>
			}</code></li>
			<li>Here we initialize our map of directions. Whenever we add a location to the queue, we'll set the direction here to be the one that points towards the location we popped off of the queue that it was adjacent to.<br>
			<code>ArrayList< ArrayList<Direction> > directions = new ArrayList<ArrayList<Direction> >();
			for(int y = 0; y < gameMap.height; y++) {<br>
				ArrayList<Direction> dRow = new ArrayList<Direction>();<br>
				for(int x = 0; x < gameMap.width; x++) {<br>
					dRow.add(Direction.STILL);<br>
				}<br>
				directions.add(dRow);<br>
			}</code></li>
			<li>Now we can add the queue for our search! We'll initialize it with the locations that we don't own, as it's those that we're trying to reach.<br>
			<code>LinkedList<Location> toVisit = new LinkedList<Location>();</code><--- LinkedList just happens to be a structure which implements queue; there are others that would work as well.<code><br>
			for(int y = 0; y < gameMap.height; y++) {<br>
				for(int x = 0; x < gameMap.width; x++) {<br>
					Location l = new Location(x, y);<br>
					Site site = gameMap.getSite(l);<br>
					if(site.owner != myID) {<br>
						toVisit.add(l);<br>
						visited.get(y).set(x, true);<br>
					}<br>
				}<br>
			}</code></li>
		</ul>
		</p>

		<p>Next we'll need to actually use this to direct our pieces. So, we'll continually pop off of the front of the queue, add the adjacent unvisited pieces, mark their directions and them as visited, and ensure that the queue isn't empty.<br>
		<code>while(!toVisit.isEmpty()) {<br>
			Location l = toVisit.remove();<br>
			visited.get(l.y).set(l.x, true);<br>
			Location n = gameMap.getLocation(l, Direction.NORTH), e = gameMap.getLocation(l, Direction.EAST), s = gameMap.getLocation(l, Direction.SOUTH), w = gameMap.getLocation(l, Direction.WEST);<br>
			if(!visited.get(n.y).get(n.x)) {<br>
				toVisit.add(n);<br>
				visited.get(n.y).set(n.x, true);<br>
				directions.get(n.y).set(n.x, Direction.SOUTH);<br>
			}<br>
			if(!visited.get(e.y).get(e.x)) {<br>
				toVisit.add(e);<br>
				visited.get(e.y).set(e.x, true);<br>
				directions.get(e.y).set(e.x, Direction.WEST);<br>
			}<br>
			if(!visited.get(s.y).get(s.x)) {<br>
				toVisit.add(s);<br>
				visited.get(s.y).set(s.x, true);<br>
				directions.get(s.y).set(s.x, Direction.NORTH);<br>
			}<br>
			if(!visited.get(w.y).get(w.x)) {<br>
				toVisit.add(w);<br>
				visited.get(w.y).set(w.x, true);<br>
				directions.get(w.y).set(w.x, Direction.EAST);<br>
			}<br>
		}</code></p>

		<p>Next, we'll go through the map. If a piece's strength is too low, we won't move it; else we'll move it as given to by our directions map.<br>
		<code>for(int y = 0; y < gameMap.height; y++) {<br>
			for(int x = 0; x < gameMap.width; x++) {<br>
				Site site = gameMap.getSite(new Location(x, y));<br>
				if(site.owner == myID) {<br>
					if(site.strength > 5 * site.production || site.strength == 255) moves.add(new Move(new Location(x, y), directions.get(y).get(x)));<br>
					else moves.add(new Move(new Location(x, y), Direction.STILL));<br>
				}<br>
			}<br>
		}</code></p>

		<p>Finally, we'll send our moves.<br>
		<code>Networking.sendFrame(moves);</code></p>

		<p>Click here to download the full source</p>

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
