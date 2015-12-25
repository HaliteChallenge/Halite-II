<?php
	if(!isset($_GET['userIDs'])) {
		echo "Incorrect HTTP parameters.";
		exit(0);
	}

	$width = $_GET['width'];
	$height = $_GET['height'];
	$userIDs = $_GET['userIDs'];

	$startGameCommand = "./Environment $width $height ";

	foreach($userIDs as $userID) {
		$botRunPath = "../../storage/bots/$userID/run.sh";
		if(!file_exists($botRunPath)) {
			echo "Run file for user ".$userID." does not exist";
			exit(0);
		}

		$startGameCommand .= "\"$botRunPath\" ";
	}
	echo $startGameCommand;
	/*exec($startGameCommand, $gameOutput);

	// Return the botIDs ordered by ranking (first to last)
	$returnArray = array();
	$numPlayers = count($userIDs);

	// Get the playerID of each ranking (playerID starts from 1, goes up, ordered based on order of start commands)
	for($a = count($gameOutput) - $numPlayers; $a < count($gameOutput); $a++) {
		$line = $gameOutput[$a];
		$start = strpos($line, "is player ") + strlen("is player ");
		$end = 	strpos($line, " named");
		$playerID = intval(substr($line, $start, $end));
		array_push($returnArray, $userIDs[$playerID-1]);
	}

	echo json_encode($returnArray);*/
?>
