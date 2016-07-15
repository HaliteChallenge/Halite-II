<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

date_default_timezone_set('America/New_York');

require_once 'API.class.php';
require_once '../lib/swiftmailer/lib/swift_required.php';

class WebsiteAPI extends API {

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

	private function getForumsID($userID) {
		$url = "http://forums.halite.io/users/by-external/{$userID}.json/?".http_build_query(array('api_key' => $this->config['forums']['apiKey'], 'api_username' => $this->config['forums']['apiUsername']));
		$contents = file_get_contents($url);
		return intval(json_decode($contents, true)['user']['id']);
	}

	private function logOutForums($forumsID) {
		$options = array('http' => array(
						'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
						'method'  => 'POST',
						'content' => http_build_query(array('api_key' => $this->config['forums']['apiKey'], 'api_username' => $this->config['forums']['apiUsername']))
		));
		file_get_contents("http://forums.halite.io/admin/users/{$forumsID}/log_out", false, stream_context_create($options));
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
		} else if(isset($_POST['verificationCode']) && isset($_POST['userID'])) {
			$user = $this->select("SELECT verificationCode FROM User WHERE userID={$_POST['userID']} LIMIT 1");
			if($user['verificationCode'] == $_POST['verificationCode']) {
				$this->insert("UPDATE User SET isVerified=1 WHERE userID={$_POST['userID']}");
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

			// Send verification email
			$verificationCode = rand(0, 9999999999);
			try{
				$transport = Swift_SmtpTransport::newInstance("smtp.gmail.com", 465, "ssl")
				->setUsername($this->config['email']['email'])
				->setPassword($this->config['email']['password']);
				$mailer = Swift_Mailer::newInstance($transport);
				$message = Swift_Message::newInstance("Halite Email Verification")
					->setFrom(array($this->config['email']['email'] => "Halite Competition"))
					->setTo(array($email));

				$this->insert("INSERT INTO User (username, email, password, verificationCode) VALUES ('$username', '$email', '$password', '$verificationCode')");
				$userID = $this->select("SELECT userID FROM User WHERE email='$email' LIMIT 1")['userID'];

				$this->insert("INSERT INTO UserExtraStats (userID) VALUES ({$userID})");

				$message->setBody("To verify your email, <a href='halite.io/website/index.php?verificationCode={$verificationCode}&userID={$userID}'>click here</a>.", 'text/html');
				$result = $mailer->send($message);
			} catch (Exception $e) {
				return "Invalid email address";
			}

			return "Success";
		}
	}

	protected function bot() {
		if(isset($_GET["userID"])) {
			return $this->select("SELECT * FROM Bot WHERE userID={$_GET["userID"]}");
		} else if(isset($_GET["active"])) {
			return $this->select("SELECT * FROM Bot WHERE compileStatus=2");
		} else if(isset($_GET["numActive"])) {
			return mysqli_query($this->mysqli, "SELECT * FROM Bot WHERE compileStatus=2")->num_rows;
		}
	}

	protected function game() {
		if(isset($_GET['botID'])) {
			$limit = isset($_GET['limit']) ? $_GET['limit'] : 5;
			$botID = $_GET['botID'];

			$gameIDArrays = $this->selectMultiple("SELECT gameID FROM GameBot WHERE botID = $botID ORDER BY gameID DESC LIMIT $limit");
			$gameArrays = array();

			// Get each game's info
			foreach ($gameIDArrays as $gameIDArray) {
				$gameID = $gameIDArray['gameID'];
				$gameArray = $this->select("SELECT * FROM Game WHERE gameID = $gameID");

				// Get information about bots
				$gameArray['bots'] = $this->selectMultiple("SELECT botID, rank FROM GameBot WHERE gameID = $gameID");
				foreach($gameArray['bots'] as &$gameBotRow) {
					// Get rid of gameID
					unset($gameBotRow['gameID']);

					// Add in username
					$userIDArray = $this->select("SELECT userID FROM Bot WHERE botID = {$gameBotRow['botID']}");
					$usernameArray = $this->select("SELECT username FROM User WHERE userID = {$userIDArray['userID']}");
					$gameBotRow['username'] = $usernameArray['username'];
				}
				array_push($gameArrays, $gameArray);
			}
			return $gameArrays;
		}
	}

	protected function botFile() {
		if(isset($_FILES['botFile']['name']) && isset($_POST['userID']) && isset($_POST['password'])) {
			$userID = $_POST['userID'];
			$password = $_POST['password'];

			$user = $this->select("SELECT isVerified FROM User WHERE userID={$userID} and password='{$password}'");
			if(count($user) == 0 || $user['isVerified'] == false) {
				return "Unverified email";
			}

			$targetPath = "../../storage/bots/{$userID}.zip";
			if(file_exists($targetPath) == true) unlink($targetPath);
			move_uploaded_file($_FILES['botFile']['tmp_name'], $targetPath);

			$this->insert("UPDATE User SET numSubmissions=numSubmissions+1 WHERE userID = $userID");

			$numSubmissionsArray = $this->select("SELECT numSubmissions FROM User WHERE userID = $userID");
			$this->insert("INSERT INTO Bot (userID, versionNumber) VALUES ($userID, {$numSubmissionsArray['numSubmissions']})");

			return "Success";
		}
	}

	protected function forums() {
		if(isset($_GET['payload']) && isset($_GET['signature']) && isset($_GET['userID']) && isset($_GET['email']) && isset($_GET['username'])) {
			$initialBase64Payload = stripcslashes($_GET['payload']);
			$signature = $_GET['signature'];
			$userID = $_GET['userID'];
			$email = $_GET['email'];
			$username = $_GET['username'];

			$correctSignature = hash_hmac("sha256", $initialBase64Payload, $this->config['sso']['secret']);

			if($correctSignature != $signature) {
				return null;
			}

			parse_str(base64_decode($initialBase64Payload), $initialPayload);
			$nonce = $initialPayload["nonce"];

			$finalBase64Payload = base64_encode(http_build_query(array(
				"nonce" => $nonce,
				"name" => $username,
				"email" => $email,
				"external_id" =>$userID
			)));
			$finalSignature = hash_hmac("sha256", $finalBase64Payload, $this->config['sso']['secret']);

			$finalQueryString = http_build_query(array(
				"sso" => $finalBase64Payload,
				"sig" => $finalSignature
			));
			$finalURL = $this->config['sso']['url']."?".$finalQueryString;
			return $finalURL;
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
			if(isset($_SESSION['userID']) && isset($_SESSION['password'])) {
				if(count($this->select("SELECT * FROM User WHERE username = '{$_SESSION['username']}' AND password = '{$_SESSION['password']}'")) != 0) {
					$this->logOutForums($this->getForumsID($_SESSION['userID']));
				}
			}
			session_destroy();
			return "Success";
		}
	}
 }

 ?>
