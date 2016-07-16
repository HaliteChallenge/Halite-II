<html lang="en">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<title>Basic Bot Tutorial</title>

	<link href="lib/bootstrap.min.css" rel="stylesheet">
	<link href="style/general.css" rel="stylesheet">
</head>
<body>
	<div class="container">
		<?php include 'includes/navbar.php'; ?>
		<div class="row">
			<div class="col-sm-12">
				<h1>Basic Bot Tutorial</h1>

				<p>The source code for each basic bot is located <a href="https://github.com/HaliteChallenge/Halite/tree/master/website/tutorials/basic">here</a>.</p>

				<h3>Prerequisites</h3>
				<p>It is highly recommended that you have followed the directions listed in our <a href="quickstart.php">quickstart tutorial</a> before following this tutorial.</p>

				<h3>Download the Tools</h3>
				<p>While working on your bot, you will want to run and visualize games locally. To do this, you will need the game environment, which runs games and outputs replay files, and the desktop visualizer, which visualizes the environment's replay files. These are located <a href="downloads.php">here</a>.</p>

				<h3>Running a Game</h3>
				<p>
					Once those are downloaded, unzip your starter package. If you are using a compiled language, such as Java or C++, please compile the <code>MyBot</code> file of the starter package. This may be done in Java like so: <code>javac MyBot.java</code> and in C++ like so: <code>g++ -std=c++11 MyBot -o MyBot</code>). We are going to run a game between two instances of the provided starter bot, which simply moves all of its pieces randomly. To do this, run the environment binary. It should prompt you for the game's dimensions (20 by 20 is perfectly reasonable) and your bot's start command. Examples of these commands include:
					<ul>
						<li>Java - <code>cd PATH_TO_BOT; java MyBot</code></li>
						<li>Python - <code>python3 PATH_TO_BOT/MyBot.py</code></li>
						<li>C++ - <code>PATH_TO_BOT/BOT_BINARY_NAME</code></li>
					</ul>
				</p>

				<p>
					In the future, it may be more convenient to pass in all of the game's arguments in one command instead of entering each argument when prompted. You may do this like so:</p>
				</p>

				<p>
					<code>./environment WIDTH HEIGHT "START_COMMAND_BOT_1" "START_COMMAND_BOT_2" ... "START_COMMAND_BOT_N"</code>
				</p>

				<p>
					Once you have started the environment and provided it with the map's dimensions and your bots' start commands, the environment should then execute your game, outputting the turn number intermittently and ending with a ranking of each participant. You should find the game's replay file, ending with the <code>hlt</code> extension and named a very large number (your system's timestamp at the end of the game), in the same directory as the environment. You can also create a Replays folder within the environment folder for organization, and the environment will store the replays there.
				</p>

				<h3>Visualizing a Game</h3>
				<p>
					To visualize the game that you just ran, run the visualizer binary. Drag the game's replay file from your file explorer into the visualizer's window. Soon a grid filled with a few of brightly colored squares this is the map at the very start of the game. For convenience, you can also run the visualizer from terminal with the path to the replay as its parameter, or set the visualizer as the default application to open hlt files.
				</p>

				<h3>Writing a Basic Bot (in Java)</h3>
				<p>
					As you can see, the included starter bot is pretty bad at playing Halite. It just moves each of its pieces randomly each turn. This logic is encoded in these lines of the starter bot.
					<pre class="prettyprint">Site site = gameMap.contents.get(y).get(x);
if(site.owner == playerTag) {
  byte dir = Direction.randomDirection();
  moves.add(new Move(new Location((short)x, (short)y), dir));
}</pre>
				</p>

				<p>
					We can very easily build on the starter bot to write a basic bot. We will write the basic bot to move each of its pieces like so:
					<ul>
						<li>If the piece's current strength is less than 5 times the production of its tile, stay still (when a piece stays still, its tile's production is added to its strength).</li>

						<li>If the previous condition is false and the piece is adjacent to a tile owned an opposing player, attack the opposing tile.</li>
						<li>Otherwise, move randomly.</li>
					</ul>
				</p>

				<p>
					This logic may easily be transferred to code like so:
					<pre class="prettyprint">Site site = gameMap.contents.get(y).get(x);
if(site.owner == playerTag) {
  byte moveDirection = Direction.randomDirection();
  if(site.strength < site.production*5) {
    moveDirection = Direction.STILL;
  } else {
    for(byte possibleDirection : Direction.CARDINALS) {
      if(gameMap.getSite(new Location(x, y), possibleDirection).owner != playerTag) {
        moveDirection = possibleDirection;
        break;
      }
    }
  }
  moves.add(new Move(new Location(x, y), moveDirection));
}</pre>
				</p>

				<p>
					Here is the full basic bot:
					<pre class="prettyprint">import java.util.ArrayList;

public class MyBot{
  public static void main(String[] args) {
    InitPackage iPackage = Networking.getInit();
    short playerTag = iPackage.playerTag;
    Map gameMap = iPackage.map;

    Networking.sendInit("JavaBot" + playerTag);

    while(true) {
      ArrayList&lt;Move> moves = new ArrayList&lt;Move>();

      gameMap = Networking.getFrame();

      for(short y = 0; y < gameMap.contents.size(); y++) {
        for(short x = 0; x < gameMap.contents.get(y).size(); x++) {
          Site site = gameMap.contents.get(y).get(x);
          if(site.owner == playerTag) {
            byte moveDirection = Direction.randomDirection();
            if(site.strength < site.production*5) {
              moveDirection = Direction.STILL;
            } else {
              for(byte d : Direction.CARDINALS) {
                if(gameMap.getSite(new Location(x, y), d).owner != playerTag) {
                  moveDirection = d;
                  break;
                }
              }
            }
            moves.add(new Move(new Location(x, y), moveDirection));
          }
        }
      }

      Networking.sendFrame(moves);
    }
  }
}</pre>
				</p>

				<p>Try running a game between two basics and compare it to the game between two starter bots.</p>

				<p><b>Note</b>: If you submit the basic bot to competition, please make sure that the main file is named <code>MyBot</code> not <code>BasicBot</code>.</p>

				<h3>What's Next?</h3>
				<p>
					There are a number of problems with the basic bot. These include but definitely are not limited to:

					<ul>
						<li>At any given point, most of the bot's pieces are moving randomly inside the bot's territory. It would be much more efficient if those pieces would go straight towards the nearest opposing tile. It would be even more efficient if those pieces considered the production loss from moving off a tile when routing themselves to the edges.</li>
						<li>The bot expands very stupidly. It doesn't prioritize expanding to opposing territory with low strength and high production.</li>
						<li>The bot decides to stop growing a piece and start moving it when that piece's strength is greater than its production times 5. That factor of 5 is completely arbitrary. The optimal value is likely not 5.</li>
						<li>When deciding whether to attack opposing tiles, neither the strength of the bot's piece nor the strength of its opponent are taken into account. It may be a better strategy to refrain from attacking under some circumstances.</li>
						<li>The basic bot loses a lot of strength to the strength cap of 255 (if two pieces combine to have a strength greater than 255, their strength is just considered to be 255).</li>
					</ul>
				</p>
			</div>
		</div>
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
					<li class="previous"><a href="quickstart.php"><span aria-hidden="true">&larr;</span> Quickstart</a></li>
					<li class="next"><a href="bfs_tutorial.php">BFS Tutorial <span aria-hidden="true">&rarr;</span></a></li>
				</ul>
			</div>
		</div>
	</footer>

	<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
	<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
	<script src="https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js"></script>
	<script src="script/backend.js"></script>
	<script src="script/general.js"></script>
</body>
</html>
