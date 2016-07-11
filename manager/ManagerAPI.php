<?php

require_once 'API.class.php';

define("REPLAYS_DIR", "../storage/replays/");
define("BOTS_DIR", "../storage/bots/");
define("INI_FILE", "../halite.ini");

class ManagerAPI extends API{

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

	// Returns the directory that holds a bot, given the bot's userID
	private function getBotFile($userID) {
		return BOTS_DIR."{$userID}.zip";
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
			$needToBeCompiled = $this->select("SELECT * FROM User WHERE status = 1 ORDER BY userID ASC");
			if(count($needToBeCompiled) > 0) {
				$this->insert("UPDATE User SET status = 2 WHERE userID = {$needToBeCompiled['userID']}");
				return array(
					"type" => "compile",
					"user" => $needToBeCompiled);
			}

			// Assign a run game tasks
			$possibleNumPlayers = array(2, 3, 4, 5);
			$numPlayers = $possibleNumPlayers[array_rand($possibleNumPlayers)];
			$players = $this->selectMultiple("SELECT * FROM User WHERE status = 3 ORDER BY rand() LIMIT $numPlayers");
			$sizes = array(10, 20, 30, 40, 50, 60);
			$size = $sizes[array_rand($sizes)];
			if(count($players) == $numPlayers) {
				return array(
					"type" => "game",
					"width" => $size,
					"height" => $size,
					"users" => $players
				);
			}
		}
	}

	// Allow worker to post the result of their compilation
	protected function compile() {
		var_dump($_POST);
		if(isset($_POST['userID']) && isset($_POST['didCompile'])) {
			$this->insert("UPDATE Worker SET numCompiles=numCompiles+1 WHERE apiKey=$this->apiKey");

			$userID = $_POST['userID'];
			$didCompile = $_POST['didCompile'];

			if($didCompile == 1) {
				$language = isset($_POST['language']) ? $_POST['language'] : "Other";
				$this->insert("UPDATE User SET status = 3, language = '$language' WHERE userID = $userID");
			} else {
				$this->insert("UPDATE User SET status = 0 WHERE userID = $userID");
			}
		}
	}

	// Allow workers to post the result of their game
	protected function game() {
		var_dump($_FILES);
		var_dump($_POST);
		// Each user in users must have a rank, playerIndex, mu, sigma, and userID
		if(isset($_POST['users']) && count($_FILES) > 0) {
			$this->insert("UPDATE Worker SET numGames=numGames+1 WHERE apiKey=$this->apiKey");
			$users = json_decode($_POST['users']);
			var_dump($users);

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
					$this->insert("DELETE FROM GameUser WHERE gameID={$game['gameID']}");
					$this->insert("DELETE FROM Game WHERE gameID={$game['gameID']}");
				}
			}

			// Store game information in db
			$this->insert("INSERT INTO Game (replayName) VALUES ('$name')");
			$gameIDArray = $this->select("SELECT gameID FROM Game WHERE replayName = '$name' LIMIT 1");
			$gameID = $gameIDArray['gameID'];

			// Update each participant's stats
			$allUsers = $this->selectMultiple("SELECT * FROM User where status=3");
			$allUserExtras = array();
			foreach($allUsers as $user) {
				array_push($allUserExtras, $this->select("SELECT * FROM UserExtraStats where userID={$user['userID']}"));
			}
			for($a = 0; $a < count($users); $a++) {
				$timeoutInt = $users[$a]->didTimeout ? 1 : 0;
				$this->insert("INSERT INTO GameUser (gameID, userID, rank, playerIndex, territoryAverage, strengthAverage, productionAverage, stillPercentage, turnTimeAverage, didTimeout) VALUES ($gameID, {$users[$a]->userID}, {$users[$a]->rank}, {$users[$a]->playerTag}, {$users[$a]->territoryAverage}, {$users[$a]->strengthAverage}, {$users[$a]->productionAverage}, {$users[$a]->stillPercentage}, {$users[$a]->turnTimeAverage}, {$timeoutInt})");

				// Cache raw game stats
				$gameStats = $this->selectMultiple("SELECT territoryAverage, strengthAverage, productionAverage, stillPercentage, turnTimeAverage, didTimeout FROM GameUser WHERE userID={$users[$a]->userID}");
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
					$this->insert("UPDATE UserExtraStats SET $statName=$averageStatValue WHERE userID = {$users[$a]->userID}");
				}

				// Game game stat rankings
				$statToRankedStat = array("territoryAverage" => "territoryRanking", "strengthAverage" => "strengthRanking", "productionAverage" => "productionRanking", "stillPercentage" => "stillRanking", "turnTimeAverage" => "turnTimeRanking", "didTimeout" => "timeoutRanking");
				foreach($statToRankedStat as $statName => $rankedStatName) {
					usort($allUserExtras, function($a, $b) use ($statName) {
						return $a[$statName] < $b[$statName];
					});
					$rank = 100000;
					for($b = 0; $b < count($allUserExtras); $b++) {
						if($allUserExtras[$b]['userID'] == $users[$a]->userID) {
							$rank = $b+1;
							break;
						}
					}
					echo "UPDATE UserExtraStats SET {$rankedStatName}={$rank} WHERE userID = {$users[$a]->userID}\n";
					$this->insert("UPDATE UserExtraStats SET {$rankedStatName}={$rank} WHERE userID = {$users[$a]->userID}");
				}

				// Add to other stats
				$this->insert("UPDATE User SET numGames=numGames+1, mu = {$users[$a]->mu}, sigma = {$users[$a]->sigma} WHERE userID = {$users[$a]->userID}");
			}
			// Update mu and sigma
			usort($allUsers, function($a, $b) {
				return $a['rank'] < $b['rank'];
			});
			$rankings = array();
			foreach($allUsers as $user) {
				array_push($rankings, $user['mu']);
				array_push($rankings, $user['sigma']);
			}
			exec("python3 updateTrueskill.py ".implode(' ', $rankings), $lines);
			foreach($lines as $line) {
				$components = explode(' ', $line);
				$allUsers['mu'] = $components[0];
				$allUsers['sigma'] = $components[1];
				$this->insert("UPDATE User SET mu={$allUsers['mu']}, sigma={$allUsers['sigma']} WHERE userID={$allUsers['userID']}");
			}

			// Update overall rank
			usort($allUsers, function($a, $b) {
				return $a['mu']-3*$a['sigma'] < $b['mu']-3*$b['sigma'];
			});
			for($userIndex = 0; $userIndex < count($allUsers); $userIndex++) {
				$rank = $userIndex+1;
				$this->insert("UPDATE User SET rank={$rank} WHERE userID={$allUsers[$userIndex]['userID']}");
			}

		}
	}

	// Allow workers to download and post bot files
	protected function botFile() {
		if(isset($_GET['userID'])) {
			$userID = $_GET['userID'];

			header("Content-disposition: attachment; filename={$userID}.zip");
			header("Content-type: application/zip");

			ob_clean();
			flush();
			readfile($this->getBotFile($userID));
			exit;
		} else if(isset($_POST['userID']) && count($_FILES) > 0) {
			$userID = $_POST['userID'];
			$key = array_keys($_FILES)[0];
			$name = basename($_FILES[$key]['name']);

			$targetPath = BOTS_DIR."{$userID}.zip";
			move_uploaded_file($_FILES[$key]['tmp_name'], $targetPath);
		} else {
			return NULL;
		}

		return "Success";
	}

	// Allow workers to get the hash of a bot file so that they know that they downloaded the file correctly
	protected function botHash() {
		if(isset($_GET['userID'])) {
			$userID = $_GET['userID'];
			if(file_exists($this->getBotFile($userID))) return array("hash" => md5_file($this->getBotFile($userID)));
			else return "Bot file does not exist";
		}
	}
}

 ?>
