<?php
	if(!isset($_GET['userIDs'])) {
		echo "Incorrect HTTP parameters.";
		exit(0);
	}

	$width = $_GET['width'];
	$height = $_GET['height'];
	$userIDs = $_GET['userIDs'];

	// Make a game folder to hold the bot folders whose name is a random string
	$gameFolder = bin2hex(mcrypt_create_iv(22, MCRYPT_DEV_URANDOM));
	exec("mkdir $gameFolder");
	exec("chmod 777 $gameFolder");

	// Copy all bot folders into game folder
	// Assemble start command
	$startGameCommand = "./Environment $width $height ";
	foreach($userIDs as $userID) {
		$storageDir = "../../storage/bots/{$userID}";
		$tempDir = "{$gameFolder}/{$userID}";

		if(!file_exists("{$storageDir}/run.sh")) {
			echo "Run file for user ".$userID." does not exist";
			exit(0);
		}

		exec("cp -a $storageDir/. $tempDir");

		$startGameCommand .= "\"$tempDir/run.sh\" ";
	}

	exec($startGameCommand, $gameOutput);

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

	echo json_encode($returnArray);

	exec("rm -r $gameFolder");
?>
