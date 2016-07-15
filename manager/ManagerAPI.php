<?php

require_once 'API.class.php';

define("REPLAYS_DIR", "../storage/replays/");
define("BOTS_DIR", "../storage/bots/");
define("INI_FILE", "../halite.ini");

class ManagerAPI extends API {

	private $mysqli = NULL;
	private $apiKey = NULL;

	// Init database, sanitize parameters, and check if worker is valid
	public function __construct($request, $origin) {
		$this->initDB();

		if($this->isValidWorker() == false) {
			echo "Not valid worker";
			exit(1);
		} else {
			$this->insert("UPDATE Worker SET lastRequestTime = now() WHERE apiKey = {$this->apiKey}");
		}
		parent::__construct($request);
	}

	private function getAPIKey() {
		$this->apiKey = NULL;
		if(isset($_GET['apiKey'])) $this->apiKey = $_GET['apiKey'];
		else if(isset($_POST['apiKey'])) $this->apiKey = $_POST['apiKey'];
	}

	// Checks HTTP parameters and request IP to make sure that the client provided a valid API key
	private function isValidWorker() {
		// Get apiKey
		$this->getAPIKey();
		if($this->apiKey == NULL) return false;

		// Get ip
		$ipAddress = $_SERVER['REMOTE_ADDR'];

		if(count($this->select("SELECT ipAddress FROM Worker WHERE ipAddress = '$ipAddress' and apiKey = $this->apiKey")) > 0) {
			return true;
		} else {
			return false;
		}
	}

	// Returns the directory that holds a bot, given the bot's botID
	private function getBotFile($botID) {
		return BOTS_DIR."{$botID}.zip";
	}

	private function getTrueskillMatchQuality($rankingValues) {
		usort($rankingValues, function($a, $b) {
			return $a['rank'] < $b['rank'];
		});
		$rankings = array();
		foreach($rankingValues as $bot) {
			array_push($rankings, $bot['mu']);
			array_push($rankings, $bot['sigma']);
		}
		exec("python3 trueskillMatchQuality.py ".implode(' ', $rankings), $lines);
		return floatval($lines[0]);
	}


	// Initializes and returns a mysqli object that represents our mysql database
	private function initDB() {
		$config = parse_ini_file(INI_FILE, true);
		$this->mysqli = new mysqli($config['database']['hostname'],
			$config['database']['username'],
			$config['database']['password'],
			$config['database']['name']);

		if (mysqli_connect_errno()) {
			echo "<br><br>There seems to be a problem with our database. Reload the page or try again later.";
			exit();
		}
	}

	private function select($sql) {
		try {
			$res = mysqli_query($this->mysqli, $sql);
			$array = mysqli_fetch_array($res, MYSQLI_ASSOC);
			return $array;
		} catch(Exception $e) {
			return array();
		}
	}

	private function selectMultiple($sql) {
		$res = mysqli_query($this->mysqli, $sql);
		$finalArray = array();

		while($temp = mysqli_fetch_array($res, MYSQLI_ASSOC)) {
			array_push($finalArray, $temp);
		}

		return $finalArray;
	}

	private function insert($sql) {
		mysqli_query($this->mysqli, $sql);
	}

	/////////////////////////API ENDPOINTS\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	// Delegate a task to a workers
	protected function task() {
		if($this->method == 'GET') {
			// Check for compile tasks
			$needToBeCompiled = $this->select("SELECT * FROM Bot WHERE compileStatus = 0 ORDER BY botID ASC");
			if(count($needToBeCompiled) > 0) {
				$this->insert("UPDATE Bot SET compileStatus = 1 WHERE botID = {$needToBeCompiled['botID']}");
				return array(
					"type" => "compile",
					"bot" => $needToBeCompiled);
			}

			// Assign a run game tasks
			$possibleNumPlayers = array(2, 3, 4, 5);
			$numPlayers = $possibleNumPlayers[array_rand($possibleNumPlayers)];
			$players = $this->selectMultiple("SELECT * FROM Bot WHERE compileStatus = 2 ORDER BY rand() LIMIT $numPlayers");

			// Pick map size
			$sizes = array(20, 30);
			$size = $sizes[array_rand($sizes)];

			// Send game task
			if(count($players) == $numPlayers) {
				return array(
					"type" => "game",
					"width" => $size,
					"height" => $size,
					"bots" => $players
				);
			}
		}
	}

	// Allow worker to post the result of their compilation
	protected function compile() {
		var_dump($_POST);
		if(isset($_POST['botID']) && isset($_POST['didCompile'])) {
			$this->insert("UPDATE Worker SET numCompiles=numCompiles+1 WHERE apiKey=$this->apiKey");

			$botID = $_POST['botID'];
			$didCompile = $_POST['didCompile'];

			if($didCompile == 1) {
				$language = isset($_POST['language']) ? $_POST['language'] : "Other";
				$this->insert("UPDATE Bot SET compileStatus = 2, language = '$language' WHERE botID = $botID");
			} else {
				$this->insert("DELETE FROM Bot WHERE botID = $botID");
			}
		}
	}

	// Allow workers to post the result of their game
	protected function game() {
		if(isset($_POST['bots']) && count($_FILES) > 0) {
			$this->insert("UPDATE Worker SET numGames=numGames+1 WHERE apiKey=$this->apiKey");

			$mapWidth = $_POST['mapWidth'];
			$mapHeight = $_POST['mapHeight'];
			$bots = json_decode($_POST['bots']);

			// Store replay file
			$fileKey = array_keys($_FILES)[0];
			$name = basename($_FILES[$fileKey]['name']);
			$targetPath = REPLAYS_DIR."{$name}";
			move_uploaded_file($_FILES[$fileKey]['tmp_name'], $targetPath);
			if(is_file($targetPath) == false) {
				echo "Did not work";
			} else {
				echo "File transfer worked!!!!!!!!!!!";
			}
			chmod($targetPath, 0777);

			// Check that we arent storing too many replay files
			$files = glob(REPLAYS_DIR.'*.*');
			$exclude_files = array('.', '..');
			if (!in_array($files, $exclude_files)) {
				array_multisort(
					array_map( 'filemtime', $files ),
					SORT_NUMERIC,
					SORT_ASC,
					$files
				);
			}
			while(count($files) > 10000) {
				unlink($files[0]);
				array_splice($files, 0, 1);
			}

			// Check that we arent stoing too many games in db
			$res = mysqli_query($this->mysqli, "SELECT * FROM Game");
			$numRows = $res->num_rows;
			$numToDelete = $numRows - 10000;
			if($numToDelete > 0) {
				$gamesToDelete = $this->selectMultiple("SELECT gameID FROM Game ORDER BY gameID LIMIT $numToDelete");
				foreach($gamesToDelete as $game) {
					$this->insert("DELETE FROM GameBot WHERE gameID={$game['gameID']}");
					$this->insert("DELETE FROM Game WHERE gameID={$game['gameID']}");
				}
			}

			// Update mu and sigma
			usort($bots, function($a, $b) { return $a->rank > $b->rank; });
			$commandArgs = array();
			foreach($bots as $bot) {
				array_push($commandArgs, $bot->mu);
				array_push($commandArgs, $bot->sigma);
			}
			exec("python3 updateTrueskill.py ".implode(' ', $commandArgs), $lines);
			for($a = 0; $a < count($bots); $a++) {
				$components = explode(' ', $lines[$a]);
				$bots['mu'] = $components[0];
				$bots['sigma'] = $components[1];
				$this->insert("UPDATE Bot SET mu={$components[0]}, sigma={$components[1]} WHERE botID={$bots[$a]->botID}");
			}

			// Store game information in db
			$this->insert("INSERT INTO Game (replayName, mapWidth, mapHeight) VALUES ('$name', $mapWidth, $mapHeight)");
			$gameIDArray = $this->select("SELECT gameID FROM Game WHERE replayName = '$name' LIMIT 1");
			$gameID = $gameIDArray['gameID'];

			// Update each participant's stats
			$allBots = $this->selectMultiple("SELECT * FROM Bot");
			for($a = 0; $a < count($bots); $a++) {
				$didTimeout = $bots[$a]->didTimeout ? 1 : 0;
				$this->insert("INSERT INTO GameBot (gameID, botID, rank, playerIndex, territoryAverage, strengthAverage, productionAverage, stillPercentage, turnTimeAverage, didTimeout) VALUES ($gameID, {$bots[$a]->botID}, {$bots[$a]->rank}, {$bots[$a]->playerTag}, {$bots[$a]->territoryAverage}, {$bots[$a]->strengthAverage}, {$bots[$a]->productionAverage}, {$bots[$a]->stillPercentage}, {$bots[$a]->turnTimeAverage}, {$didTimeout})");

				// Cache raw game stats
				$gameStats = $this->selectMultiple("SELECT territoryAverage, strengthAverage, productionAverage, stillPercentage, turnTimeAverage, didTimeout FROM GameBot WHERE botID={$bots[$a]->botID}");
				$totalGameStats = array();
				foreach($gameStats as $oneGameStats) {
					foreach($oneGameStats as $statName => $statValue) {
						if(!array_key_exists($statName, $totalGameStats)) {
							$totalGameStats[$statName] = 0;
						}
						$totalGameStats[$statName] += $statValue;
					}
				}
				foreach($totalGameStats as $statName => $totalStatValue) {
					$averageStatValue = $totalStatValue / count($gameStats);
					$this->insert("UPDATE Bot SET $statName=$averageStatValue WHERE botID = {$bots[$a]->botID}");
				}

				// Game game stat rankings
				$statToRankedStat = array("territoryAverage" => "territoryRanking", "strengthAverage" => "strengthRanking", "productionAverage" => "productionRanking", "stillPercentage" => "stillRanking", "turnTimeAverage" => "turnTimeRanking", "didTimeout" => "timeoutRanking");
				foreach($statToRankedStat as $statName => $rankedStatName) {
					usort($allBots, function($bot1, $bot2) use ($statName) {
						return $bot1[$statName] < $bot2[$statName];
					});
					$rank = 100000;
					for($b = 0; $b < count($allBots); $b++) {
						if($allBots[$b]['botID'] == $bots[$a]->botID) {
							$rank = $b+1;
							break;
						}
					}
					$this->insert("UPDATE Bot SET {$rankedStatName}={$rank} WHERE botID = {$bots[$a]->botID}");
				}

				// Add to other stats
				$this->insert("UPDATE Bot SET numGames=numGames+1, mu = {$bots[$a]->mu}, sigma = {$bots[$a]->sigma} WHERE botID = {$bots[$a]->botID}");
			}

			// Update overall rank
			usort($allBots, function($bot1, $bot2) {
				return $bot1['mu']-3*$bot1['sigma'] < $bot2['mu']-3*$bot2['sigma'];
			});
			for($botIndex = 0; $botIndex < count($allBots); $botIndex++) {
				$rank = $botIndex+1;
				$this->insert("UPDATE Bot SET rank={$rank} WHERE botID={$allBots[$botIndex]['botID']}");
			}

		}
	}

	// Allow workers to download and post bot files
	protected function botFile() {
		if(isset($_GET['botID'])) {
			$botID = $_GET['botID'];

			header("Content-disposition: attachment; filename={$botID}.zip");
			header("Content-type: application/zip");

			ob_clean();
			flush();
			readfile($this->getBotFile($botID));
			exit;
		} else if(isset($_POST['botID']) && count($_FILES) > 0) {
			$botID = $_POST['botID'];
			$key = array_keys($_FILES)[0];
			$name = basename($_FILES[$key]['name']);

			$targetPath = BOTS_DIR."{$botID}.zip";
			move_uploaded_file($_FILES[$key]['tmp_name'], $targetPath);
		} else {
			return NULL;
		}

		return "Success";
	}

	// Allow workers to get the hash of a bot file so that they know that they downloaded the file correctly
	protected function botHash() {
		if(isset($_GET['botID'])) {
			$botID = $_GET['botID'];
			if(file_exists($this->getBotFile($botID))) return array("hash" => md5_file($this->getBotFile($botID)));
			else return "Bot file does not exist";
		}
	}
}

 ?>
