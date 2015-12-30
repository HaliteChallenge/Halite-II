<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

require_once '../API.class.php';
class ManagerAPI extends API
{

	// The database
	private $mysqli = NULL;

	public function __construct($request, $origin) {
		$this->initDB();

		$this->sanitizeHTTPParameters();

		if($this->isValidWorker() == false) {
			echo "Not valid worker";
			exit(1);
		}

		parent::__construct($request);
	}

	private function isValidWorker() {
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

	private function escapeString($var) {
		if(is_string($var)) return $this->mysqli->real_escape_string($var);
		else return $var;
	}
	
	private function sanitizeHTTPParameters() {
		$_GET = array_map(array($this, "escapeString"), $_GET);
		$_POST = array_map(array($this, "escapeString"), $_POST);
	}

	private function getBotDirectory($userID) {
		return "../../../storage/bots/$userID";
	}

	private function getBotFile($userID) {
		$botDirectory = $this->getBotDirectory($userID);
		$file = NULL;
		
		if (file_exists("{$botDirectory}/bot.zip")) $file = "{$botDirectory}/bot.zip";
		else if (file_exists("{$botDirectory}/bot.tgz")) $file = "{$botDirectory}/bot.tgz";
		else if (file_exists("{$botDirectory}/bot.tar.gz")) $file = "{$botDirectory}/bot.tar.gz";
		else if (file_exists("{$botDirectory}/bot.tar.xz")) $file = "{$botDirectory}/bot.tar.xz";
		else if (file_exists("{$botDirectory}/bot.txz")) $file = "{$botDirectory}/bot.txz";
		else if (file_exists("{$botDirectory}/bot.tar.bz2")) $file = "{$botDirectory}/bot.tar.bz2";
		else if (file_exists("{$botDirectory}/bot.tbz")) $file = "{$botDirectory}/bot.tbz";

		return $file;
	}

	// Initializes and returns a mysqli object that represents our mysql database
	private function initDB() {
		$config = include("../config.php");
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

	// API ENDPOINTS
	/*-----------------------------------------------------------------------*/
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

			// Run a game
			$users = $this->selectMultiple("SELECT userID FROM User");

			$userIDs = array();
			$numPlayers = 2;

			for($a = 0; $a < $numPlayers; $a++) {
				$key = array_rand($users);
				array_push($userIDs, $users[$key]['userID']);
				unset($users[$key]);
			}

			return array(
				"type" => "game",
				"width" => 10,
				"height" => 10,
				"userIDs" => $userIDs);
			
		}
		return NULL;
	}

	protected function compile() {
		if(isset($_POST['userID']) && isset($_POST['didCompile'])) {
			$userID = $_POST['userID'];
			$didCompile = $_POST['didCompile'];

			if($didCompile) {
				$language = isset($_POST['language']) ? $_POST['language'] : "Other";
				$this->insert("UPDATE User SET status = 3, language = '$language' WHERE userID = $userID");
			} else {
				$this->insert("UPDATE User SET status = 0 WHERE userID = $userID");
			}
		}
	}

	protected function game() {
		if(isset($_POST['rankedUserIDs']) && isset($_POST['rankedScores']) && count($_FILES) > 0) {
			$rankedUserIDs = $_POST['rankedUserIDs'];			
			$rankedScores = $_POST['rankedScores'];			

			// Store replay file
			$fileKey = array_keys($_FILES)[0];
			$name = basename($_FILES[$fileKey]['name']);
			$targetPath = "../../../storage/replays/{$name}";
			move_uploaded_file($_FILES[$fileKey]['tmp_name'], $targetPath);
			
			// Store game infromation in db
			$this->insert("INSERT INTO Game (replayName) VALUES ('$name')");
			$gameIDArray = $this->select("SELECT gameID FROM Game WHERE replayName = '$name' LIMIT 1");
			$gameID = $gameIDArray['gameID'];
			
			for($a = 0; $a < count($rankedUserIDs); $a++) {
				$userID = $rankedUserIDs[$a];
				$score = $rankedScores[$a];
				$this->insert("INSERT INTO GameUser (gameID, userID, rank, score) VALUES ($gameID, $userID, $a, $score)");
			}
		}
	}

	protected function botFile() {
		if(isset($_GET['userID'])) {
			$userID = $_GET['userID'];
			$botDirectory = $this->getBotDirectory($userID);

			if (file_exists("{$botDirectory}/bot.zip")) {
				header("Content-disposition: attachment; filename=bot.zip");
				header("Content-type: application/zip");
			} else if (file_exists("{$botDirectory}/bot.tgz")) {
				header("Content-disposition: attachment; filename=bot.tgz");
				header("Content-type: application/x-gtar");
			} else if (file_exists("{$botDirectory}/bot.tar.gz")) {
				header("Content-disposition: attachment; filename=bot.tgz");
				header("Content-type: application/x-gtar");
			} else if (file_exists("{$botDirectory}/bot.tar.xz")) {
				header("Content-disposition: attachment; filename=bot.txz");
				header("Content-type: application/x-gtar");
			} else if (file_exists("{$botDirectory}/bot.txz")) {
				header("Content-disposition: attachment; filename=bot.txz");
				header("Content-type: application/x-gtar");
			} else if (file_exists("{$botDirectory}/bot.tar.bz2")) {
				header("Content-disposition: attachment; filename=bot.tbz");
				header("Content-type: application/x-gtar");
			} else if (file_exists("{$botDirectory}/bot.tbz")) {
				header("Content-disposition: attachment; filename=bot.tbz");
				header("Content-type: application/x-gtar");
			} else {
				header("HTTP/1.0 404 Not Found");
				die("Could not find file");
			}

			ob_clean();
			flush();
			readfile($this->getBotFile($userID));
			exit;
		} else if(isset($_POST['userID']) && count($_FILES) > 0) {
			$userID = $_POST['userID'];
			$key = array_keys($_FILES)[0];
			$name = basename($_FILES[$key]['name']);

			$targetPath = "../../../storage/bots/{$userID}/{$name}";
			move_uploaded_file($_FILES[$key]['tmp_name'], $targetPath);
		} else {
			var_dump($_FILES);
			return NULL;
		}

		return "Success";
	}

	protected function botHash() {
		if(isset($_GET['userID'])) {
			$userID = $_GET['userID'];
			return array("hash" => md5_file($this->getBotFile($userID)));
		}
	}
 }

 ?>
