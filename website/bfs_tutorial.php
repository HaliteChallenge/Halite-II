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

		<h1>Tutorial: Breadth-First Search</h1>

        <h3>Introduction</h3>
        <p>The Basic Bot we wrote in the previous tutorial was certainly a big step up on the random bot provided in the starter packages, but we can do much better. The central issue with the Basic Bot is that although it utilizes pieces on its borders, pieces that are not on borders just move randomly once they have a fairly high strength. This is hugely inefficient for a few reasons:
        <ul>
        	<li>The bot will often accidentally move pieces whose strengths some to greater than 255 into each other. Due to the strength cap, this lowers the overall strength of the bot.</li>
        	<li>The bot doesn't effectively utilize its strength to expand or attack its opponents. It simply relies on a piece coming into a border by random chance, and when a piece is more than a few squares away from a border this very rarely happens. These pieces are not appreciably helping the bot to win the game.</li>
        	<li>The bot doesn't move pieces still as often as it could. To minimize overflow waste from the 255 cap, the bot limits how long it will have pieces remain still before it begins to move them randomly. An algorithm better at moving strength to the borders of a bot would allow the bot to make more still move, which translates to a higher production efficiency and more overall strength produced by the bot.</li>
        </ul></p>

        
	</div>

	<footer class="footer pageContent">
		<div class="container">
			<div id="footer">
				<ul class="pager">
          <li class="previous"><a href="contest_spec.php"><span aria-hidden="true">&larr;</span> Contest Spec</a></li>
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
