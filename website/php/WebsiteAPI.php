<?php

require_once 'API.class.php';
require_once '../lib/swiftmailer/lib/swift_required.php';

class WebsiteAPI extends API
{

	// The database
	private $mysqli = NULL;

	public function __construct($request, $origin) {
		$this->config = parse_ini_file("../../halite.ini", true);

		$this->initDB();

		$this->sanitizeHTTPParameters();

		parent::__construct($request);
	}

	private function sanitizeHTTPParameters() {
		foreach ($_GET as $key => $value) {
			$_GET[$key] = $this->mysqli->real_escape_string($value);
		}
		foreach ($_POST as $key => $value) {
			$_POST[$key] = $this->mysqli->real_escape_string($value);
		}
	}

	private function encryptPassword($password) {
		return $this->mysqli->real_escape_string(crypt($password, $this->config['encrypt']['salt']));
	}

	// Initializes and returns a mysqli object that represents our mysql database
	private function initDB() {
		$this->mysqli = new mysqli($this->config['database']['hostname'],
			$this->config['database']['username'],
			$this->config['database']['password'],
			$this->config['database']['name']);

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
		if(isset($_GET["username"])) {
			if(isset($_GET["password"])) {
				$password = $this->encryptPassword($_GET['password']);
				return $this->select("SELECT * FROM User WHERE username = '{$_GET['username']}' AND password = '$password'");
			} else {
				$fields = $this->select("SELECT * FROM User WHERE username = '{$_GET['username']}'");
				unset($fields["password"]);
				return $fields;
			}
		} else if (isset($_GET["userID"])) {
			if(isset($_GET["password"])) {
				$password = $this->encryptPassword($_GET['password']);
				return $this->select("SELECT * FROM User WHERE userID = '{$_GET['userID']}' AND password = '{$password}'");
			} else {
				$fields = $this->select("SELECT * FROM User WHERE userID = '{$_GET['userID']}'");
				unset($fields["password"]);
				return $fields;
			}
		} else if(isset($_GET['active'])) {
			$results = $this->selectMultiple("SELECT * FROM User WHERE status = 3");
			foreach(array_keys($results) as $key) unset($results[$key]["password"]);
			return $results;
		} else if(isset($_PUT['verificationCode']) && isset($_PUT['userID'])) {
			if($this->select("SELECT isVerified FROM User WHERE userID={$_GET['userID']} LIMIT 1")['verificationCode'] == $_GET['verificationCode']) {
				$this->insert("UPDATE User SET isVerified=1 WHERE userID={$_GET['userID']}");
				return "Success";
			}
			return "Fail";
		} else if (isset($_POST["username"]) && isset($_POST["email"]) && isset($_POST["password"])) {
			$username = $_POST["username"];
			$email = $_POST["email"];
			$password = $this->encryptPassword($_POST["password"]);

			$usernameArray = $this->select("SELECT username FROM User WHERE username = '$username' LIMIT 1");
			if(isset($usernameArray['username'])) {
				return "Username already exists";
			}

			$emailArray = $this->select("SELECT email FROM User WHERE email = '$email' LIMIT 1");
			if(isset($emailArray['email'])) {
				return "Email already exists";
			}

			$this->insert("INSERT INTO User (username, email, password, mu, sigma, status, verificationCode) VALUES ('$username', '$email', '$password', 25.000, 8.333, 0, '".rand(0, 9999999999)."')");

			// Send verification email
			$transport = Swift_SmtpTransport::newInstance('smtp.gmail.com', 465, "ssl")
			->setUsername($this->config['email']['email'])
			->setPassword($this->config['email']['password']);
			$mailer = Swift_Mailer::newInstance($transport);
			$message = Swift_Message::newInstance('Halite Email Verification')
				->setFrom(array($this->config['email']['email'] => 'Halite Competition'))
				->setTo(array($email))
				->setBody('To verify you email, <a href="#">click here</a>.', 'text/html');
			$result = $mailer->send($message);

			return "Success";
		}
	}

	protected function game() {
		if(isset($_GET['userID'])) {
			$limit = isset($_GET['limit']) ? $_GET['limit'] : 5;
			$userID = $_GET['userID'];

			$gameIDArrays = $this->selectMultiple("SELECT * FROM GameUser WHERE userID = $userID ORDER BY gameID DESC LIMIT $limit");
			$gameArrays = array();

			// Get each game's info
			foreach ($gameIDArrays as $gameIDArray) {
				$gameID = $gameIDArray['gameID'];
				$gameArray = $this->select("SELECT * FROM Game WHERE gameID = $gameID");

				// Get information about users
				$gameArray['users'] = $this->selectMultiple("SELECT * FROM GameUser WHERE gameID = $gameID");
				foreach($gameArray['users'] as &$gameUserRow) {
					// Get rid of gameID
					unset($gameUserRow['gameID']);

					// Add in user info
					$userInfo = $this->select("SELECT * FROM User WHERE userID = {$gameUserRow['userID']}");
					foreach($userInfo as $key => $value) $gameUserRow[$key] = $value;
				}
				array_push($gameArrays, $gameArray);
			}
			return $gameArrays;
		}
	}

	protected function botFile() {
		if(isset($_FILES['botFile']['name']) && isset($_POST['userID'])) {
			$userID = $_POST['userID'];

			$user = $this->select("SELECT isVerified FROM User WHERE userID={$_POST['userID']}");
			if(count($user) == 0 || $user['isVerified'] == false) {
				return "Unverified email";
			}

			$targetPath = "../../storage/bots/{$userID}.zip";
			if(file_exists($targetPath) == true) unlink($targetPath);

			move_uploaded_file($_FILES['botFile']['tmp_name'], $targetPath);

			$this->insert("UPDATE User SET status = 1, mu = 25.000, sigma = 8.333 WHERE userID = $userID");

			return "Success";
		}
	}

	protected function session() {
		session_start();
		if($this->method == 'GET') {
			if(count($_SESSION) > 0) return $_SESSION;
			else return NULL;
		} else if(isset($_POST['username']) & isset($_POST['password'])) {
			$username = $_POST['username'];
			$password = $this->encryptPassword($_POST['password']);

			$user = $this->select("SELECT * FROM User WHERE username = '$username' AND password = '$password'");
			if($user['isVerified'] == false) {
				return "Unverified user";
			}
			$_SESSION = $user;
			return "Success";
		} else if(isset($_POST['userID']) & isset($_POST['password'])) {
			$userID = $_POST['userID'];
			$password = $this->encryptPassword($_POST['password']);

			$user = $this->select("SELECT * FROM User WHERE userID = $userID AND password = '$password'");
			if($user['isVerified'] == false) {
				return "Unverified user";
			}
			$_SESSION = $user;
			return "Success";
		} else if($this->method == 'DELETE') {
			session_destroy();
			return "Success";
		}
	}
 }

 ?>
