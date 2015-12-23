<?php

require_once 'API.class.php';
class HaliteAPI extends API
{

	// The database
	private $mysqli = NULL;

	public function __construct($request, $origin) {
		$this->initDB();
		parent::__construct($request);
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
		$res = mysqli_query($this->mysqli, $sql);
		return mysqli_fetch_array($res, MYSQLI_ASSOC);
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
	// Endpoint associated with a users credentials (everything in the User table; i.e. name, email, firstname, etc.)
	protected function user() {
		if(isset($_GET["username"]) && isset($_GET["password"])) {
			$username = $_GET["username"];
			$password = $_GET["password"];
			return $this->select("SELECT * FROM User WHERE username = '$username' AND password = '$password'");
		} else if (isset($_GET["userID"]) && isset($_GET["password"])) {
			$userID = $_GET["userID"];
			$password = $_GET["password"];
			return $this->select("SELECT * FROM User WHERE userID = $userID AND password = '$password'");
		} else if (isset($_GET["username"])) {
			$username = $_GET["username"];
			return $this->select("SELECT userID, username FROM User WHERE username = '$username'");
		} else if (isset($_GET["userID"])) {
			$userID = $_GET["userID"];
			return $this->select("SELECT userID, username FROM User WHERE userID = $userID");
		} else if (isset($_POST["username"]) && isset($_POST["password"])) {
			$username = $_POST["username"];
			$password = $_POST["password"];

			$usernameArray = $this->select("SELECT username FROM User WHERE username = '$username' LIMIT 1");
			if(isset($usernameArray['username'])) {
				return NULL;
			}

			$this->insert("INSERT INTO User (username, password) VALUES ('$username', '$password')");
		} else {
			return "No endpoint reached";
		}

		return "Success";
	}

	protected function bot() {
		if(isset($_GET["userID"])) {
			$userID = $_GET["userID"];

			return $this->select("SELECT * FROM Bot WHERE userID = $userID");
		} else if(isset($_GET["botID"])) {
			$botID = $_GET["botID"];

			return $this->select("SELECT * FROM Bot WHERE botID = $botID");
		} else if(isset($_POST['name']) && isset($_POST['userID'])) {
			$name = $_POST['name'];
			$userID = $_POST['userID'];

			$botNameArray = $this->select("SELECT name FROM Bot WHERE name = '$name' LIMIT 1");
			if(isset($botNameArray['name'])) {
				return NULL;
			}

			$this->insert("INSERT INTO Bot (userID, name) VALUES ($userID, '$name')");
		} else {
			return "No endpoint reached";
		}

		return "Success";
	}

	protected function botFiles() {
		if(isset($_FILES['files']['name']) && isset($_POST['userID'])) {
			$userID = $_POST['userID'];

			$targetPath = "../../storage/bots/$userID";
			if(file_exists($targetPath) == true) {
				$it = new RecursiveDirectoryIterator($targetPath, RecursiveDirectoryIterator::SKIP_DOTS);
				$files = new RecursiveIteratorIterator($it,
				             RecursiveIteratorIterator::CHILD_FIRST);
				foreach($files as $file) {
				    if ($file->isDir()){
				        rmdir($file->getRealPath());
				    } else {
				        unlink($file->getRealPath());
				    }
				}
			} else {
				mkdir($targetPath);
			}
			
			for($i = 0; $i < count($_FILES['files']['name']); $i++){
				$finalPath = $targetPath."/".$_FILES['files']['name'][$i];
				move_uploaded_file($_FILES['files']['tmp_name'][$i], $finalPath);
			}
		} else {
			return "No endpoint reached";
		}
		return "Success";
	}

	protected function session() {
		session_start();
		if($this->method == 'GET') {
			if(count($_SESSION) > 0) return $_SESSION;
			else return NULL;
		} else if(isset($_POST['username']) & isset($_POST['password'])) {
			$username = $_POST['username'];
			$password = $_POST['password'];

			$_SESSION['username'] = $username;
			$_SESSION['password'] = $password;
			$userIDArray = $this->select("SELECT userID FROM User WHERE username = '$username' AND password = '$password'");
			$_SESSION['userID'] = $userIDArray['userID'];
		} else if(isset($_POST['userID']) & isset($_POST['password'])) {
			$userID = $_POST['userID'];
			$password = $_POST['password'];

			$_SESSION['userID'] = $userID;
			$_SESSION['password'] = $password;
			$usernameArray = $this->select("SELECT username FROM User WHERE userID = $userID AND password = '$password'");
			$_SESSION['username'] = $usernameArray['username'];
		} else if($this->method == 'DELETE') {
			session_destroy();
		} else {
			return "No endpoint reached";
		}

		return "Success";
	}
 }

 ?>