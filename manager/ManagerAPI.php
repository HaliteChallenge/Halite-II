<?php

// FOR DEBUGGING PURPOSES
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

require_once 'API.class.php';
class ManagerAPI extends API
{

	private $mysqli = NULL;

	// Init database, sanitize parameters, and check if worker is valid
	public function __construct($request, $origin) {
		$this->initDB();

		//$this->sanitizeHTTPParameters();

		if($this->isValidWorker() == false) {
			echo "Not valid worker";
			exit(1);
		}

		parent::__construct($request);
	}

	// Checks HTTP parameters and request IP to make sure that the client provided a valid API key
	private function isValidWorker() {
		return true;
		// Get apiKey
		$apiKey = NULL;
		if(isset($_GET['apiKey'])) $apiKey = $_GET['apiKey'];
		else if(isset($_POST['apiKey'])) $apiKey = $_POST['apiKey'];
		else if(isset($_PUT['apiKey'])) $apiKey = $_PUT['apiKey'];
		else if(isset($_DELETE['apiKey'])) $apiKey = $_DELETE['apiKey'];

		if($apiKey == NULL) return false;

		// Get ip
		$ipAddress = $_SERVER['REMOTE_ADDR'];

		if(count($this->select("SELECT ipAddress FROM Worker WHERE ipAddress = '$ipAddress' and apiKey = $apiKey")) > 0) return true;
		else return false;
	}

	private function sanitizeHTTPParameters() {
		foreach ($_GET as $key => $value) {
			$_GET[$key] = escapeshellcmd($this->mysqli->real_escape_string($value));
		}
		foreach ($_POST as $key => $value) {
			$_POST[$key] = escapeshellcmd($this->mysqli->real_escape_string($value));
		}
	}

	// Returns the directory that holds a bot, given the bot's userID
	private function getBotFile($userID) {
		return "../../../storage/bots/{$userID}.zip";
	}

	// Initializes and returns a mysqli object that represents our mysql database
	private function initDB() {
		$config = include("config.php");
		$this->mysqli = new mysqli($config['hostname'],
			$config['username'],
			$config['password'],
			$config['databaseName']);

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
			$needToBeCompiled = $this->select("SELECT userID FROM User WHERE status = 1 ORDER BY userID ASC");
			if(count($needToBeCompiled) > 0) {
				$this->insert("UPDATE User SET status = 2 WHERE userID = {$needToBeCompiled['userID']}");
				return array(
					"type" => "compile",
					"userID" => $needToBeCompiled['userID']);
			}

			// Assign a run game tasks
			$numPlayers = 2;
			$players = $this->selectMultiple("SELECT userID, mu, sigma FROM User WHERE status = 3 ORDER BY rand() LIMIT $numPlayers");
			$sizes = array(10, 20, 30);
			$size = $sizes[array_rand($sizes)];
			if(count($players) == 2) {
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
		// Each user in users must have a rank, playerIndex, score, mu, sigma, and userID
		if(isset($_POST['users']) && count($_FILES) > 0) {
			$users = json_decode($_POST['users']);
			var_dump($users);
			// Store replay file
			$fileKey = array_keys($_FILES)[0];
			$name = basename($_FILES[$fileKey]['name']);
			$targetPath = "../../../storage/replays/{$name}";
			move_uploaded_file($_FILES[$fileKey]['tmp_name'], $targetPath);
			if(is_file($targetPath) == false) {
				echo "Did not work";
			} else {
				echo "File transfer worked!!!!!!!!!!!";
			}
			chmod($targetPath, 0777);

			// Store game information in db
			$this->insert("INSERT INTO Game (replayName) VALUES ('$name')");
			$gameIDArray = $this->select("SELECT gameID FROM Game WHERE replayName = '$name' LIMIT 1");
			$gameID = $gameIDArray['gameID'];

			for($a = 0; $a < count($users); $a++) {
				$this->insert("INSERT INTO GameUser (gameID, userID, rank, score, playerIndex) VALUES ($gameID, {$users[$a]->userID}, {$users[$a]->rank}, {$users[$a]->score}, {$users[$a]->playerIndex})");
				$this->insert("UPDATE User SET mu = {$users[$a]->mu}, sigma = {$users[$a]->sigma} WHERE userID = {$users[$a]->userID}");
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

			$targetPath = "../../../storage/bots/{$userID}.zip";
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
