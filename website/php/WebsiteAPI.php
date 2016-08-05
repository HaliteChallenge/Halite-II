<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
ini_set('session.gc_maxlifetime', 7*24*3600);

header('Access-Control-Allow-Origin: *');
error_reporting(E_ALL);

date_default_timezone_set('America/New_York');

include dirname(__FILE__).'/API.class.php';
include dirname(__FILE__).'/../lib/swiftmailer/lib/swift_required.php';

define("INI_PATH", dirname(__FILE__)."/../../halite.ini");
define("BOTS_PATH", dirname(__FILE__)."/../../storage/bots/");
define("ERRORS_PATH", dirname(__FILE__)."/../../storage/errors/");
define("REPLAYS_PATH", dirname(__FILE__)."/../../storage/replays/");

class WebsiteAPI extends API{
	private $TS_CDIRS = array("213.86.80.152/29", "208.77.212.0/22");
	private $TS_WIFI_IPS = array("213.86.80.153", "208.77.215.155", "208.77.214.155");

	// The database
	private $mysqli = NULL;

	public function __construct($request) {
		$this->config = parse_ini_file(INI_PATH, true);

		$this->initDB();

		$this->sanitizeHTTPParameters();

		parent::__construct($request);
	}
	
	/* Apply MYSQL sanitization to all incoming parameters
	 * 
	 * TODO: take this out, switch to just sanitizing when necessary
	 * Sanitizing everything breaks json parsing of params
	 * Also, bad practice
	 */
	private function sanitizeHTTPParameters() {
		foreach ($_GET as $key => $value) {
			$_GET[$key] = $this->mysqli->real_escape_string($value);
		}
		foreach ($_POST as $key => $value) {
			$_POST[$key] = $this->mysqli->real_escape_string($value);
		}
	}
	
	// Encript our password as a salted hash
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
	
	// Gets the id of one of our users in the discourse forums system
	private function getForumsID($userID) {
		$url = "http://forums.halite.io/users/by-external/{$userID}.json/?".http_build_query(array('api_key' => $this->config['forums']['apiKey'], 'api_username' => $this->config['forums']['apiUsername']));
		$contents = file_get_contents($url);
		return intval(json_decode($contents, true)['user']['id']);
	}
	
	// Log a users out of forums.halite.io
	private function logOutForums($forumsID) {
		$options = array('http' => array(
			'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
			'method'  => 'POST',
			'content' => http_build_query(array('api_key' => $this->config['forums']['apiKey'], 'api_username' => $this->config['forums']['apiUsername']))
		));
		file_get_contents("http://forums.halite.io/admin/users/{$forumsID}/log_out", false, stream_context_create($options));
	}
	
	// Returns true if an ip is in one of the specified cdir blocks
	private function testUserIP($user_ip, $cidrs) {
		$ipu = explode('.', $user_ip);
		foreach ($ipu as &$v)
			$v = str_pad(decbin($v), 8, '0', STR_PAD_LEFT);
		$ipu = join('', $ipu);
		$res = false;
		foreach ($cidrs as $cidr) {
			$parts = explode('/', $cidr);
			$ipc = explode('.', $parts[0]);
			foreach ($ipc as &$v) $v = str_pad(decbin($v), 8, '0', STR_PAD_LEFT);
			$ipc = substr(join('', $ipc), 0, $parts[1]);
			$ipux = substr($ipu, 0, $parts[1]);
			$res = ($ipc === $ipux);
			if ($res) break;
		}
		return $res;
	}
	
	// Select one element from our MySQL db
	private function select($sql) {
		$res = mysqli_query($this->mysqli, $sql);
		return mysqli_fetch_array($res, MYSQLI_ASSOC);
	}
	
	// Select all available elements from our MySQL db
	private function selectMultiple($sql) {
		$res = mysqli_query($this->mysqli, $sql);
		$finalArray = array();

		while($temp = mysqli_fetch_array($res, MYSQLI_ASSOC)) {
			array_push($finalArray, $temp);
		}

		return $finalArray;
	}
	
	// Perform a non-select (insert, update, delete) query
	// TODO: insert is a terrible name for this method
	private function insert($sql) {
		mysqli_query($this->mysqli, $sql);
	}

	//------------------------------------- API ENDPOINTS ----------------------------------------\\
	// Endpoint associated with a users credentials (everything in the User table; i.e. name, email, firstname, etc.)
	// -------------------------------------------------------------------------------------------\\

	/* User Endpoint
	 *
	 * Encapsulates user information.
	 * Hides a user's hashed password, email, and verificationCode,
	 * if no authentication is provided.
	 */	
	protected function user() {
		// Get a user's info with a username		
		if(isset($_GET["username"])) {
			if(isset($_GET["password"])) {
				$password = $this->encryptPassword($_GET['password']);
				return $this->select("SELECT * FROM User WHERE username = '{$_GET['username']}' AND password = '$password'");
			} else {
				$fields = $this->select("SELECT * FROM User WHERE username = '{$_GET['username']}'");
				unset($fields["password"]);
				unset($fields["email"]);
				unset($fields["verificationCode"]);
				return $fields;
			}
		} 
		
		// Get a user's info with a userID
		else if (isset($_GET["userID"])) {
			if(isset($_GET["password"])) {
				$password = $this->encryptPassword($_GET['password']);
				return $this->select("SELECT * FROM User WHERE userID = '{$_GET['userID']}' AND password = '{$password}'");
			} else {
				$fields = $this->select("SELECT * FROM User WHERE userID = '{$_GET['userID']}'");
				unset($fields["password"]);
				unset($fields["email"]);
				unset($fields["verificationCode"]);
				return $fields;
			}
		} 
		
		// Get all of the user's with active submissions
		else if(isset($_GET['active'])) {
			$results = $this->selectMultiple("SELECT * FROM User WHERE status = 3");
			foreach(array_keys($results) as $key) {
				unset($results[$key]["password"]);
				unset($results[$key]["email"]);
				unset($results[$key]["verificationCode"]);
			}
			return $results;
		} 

		// Verify an email
		else if(isset($_POST['verificationCode']) && isset($_POST['userID'])) {
			$user = $this->select("SELECT verificationCode FROM User WHERE userID={$_POST['userID']} LIMIT 1");
			if($user['verificationCode'] == $_POST['verificationCode']) {
				$this->insert("UPDATE User SET isVerified=1 WHERE userID={$_POST['userID']}");
				return "Success";
			}
			return "Fail";
		} 
		
		// Register a new halite account. Send the verification email
		else if (isset($_POST["username"]) && isset($_POST["email"]) && isset($_POST["password"])) {
			$username = htmlspecialchars($_POST["username"]);
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

			$explodedEmail = explode("@", $email);
			if(count($explodedEmail) != 2) {
				return "Invalid email address";
			}
			if(strcmp($explodedEmail[1], "twosigma.com") != 0) {
				return "Email is not two sigma email";
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

				$this->insert("INSERT INTO User (username, email, password, mu, sigma, status, verificationCode) VALUES ('$username', '$email', '$password', 25.000, 8.333, 0, '$verificationCode')");
				$userID = $this->select("SELECT userID FROM User WHERE email='$email' LIMIT 1")['userID'];

				$this->insert("INSERT INTO UserExtraStats (userID) VALUES ({$userID})");

				$message->setBody("<html><body>To verify your email, <a href='http://halite.io/website/index.php?verificationCode={$verificationCode}&userID={$userID}'>click here</a>.</body></html>", 'text/html');
				$result = $mailer->send($message);
			} catch (Exception $e) {
				return "Invalid email address";
			}

			return "Success";
		}
	}

	/* Extra Stats Endpoint
	 *
	 * We store a number of agreggated stats about each bot.
	 * These are currently not stored in the User table.
	 * 
	 * TODO:The original rationale was that putting all of these stats in the User table would make it too latent;
	 * However, the separation of the extra stats and the user's base info is kind of an arbitrary one.
	 * It would be much nicer to decouple 'bot' information from 'user' information.
	 * This would allow for the quick addition of mutiple bots per user and is less arbitrary.
	 */
	function extraStats() {
		if(isset($_GET["userID"])) {
			return $this->select("SELECT * FROM UserExtraStats WHERE userID={$_GET["userID"]}");
		}
	}

	/* User History Endpoint
	 *
	 * We store the a history of user's bot submissions.
	 * This mitigates the 'stealth' submission problem, 
	 * where users submit a bot, wait for their rank to stabilize, and take it down for the sake of secrecy.
	 */
	protected function history() {
		if(isset($_GET["userID"])) {
			return $this->selectMultiple("SELECT * FROM UserHistory WHERE userID={$_GET["userID"]} ORDER BY versionNumber DESC");
		}
	}

	/* Game Endpoint
	 *
	 * Games are continuously run on our servers and exposed on our website
	 */
	protected function game() {
		if(isset($_GET['userID'])) {
			$limit = isset($_GET['limit']) ? $_GET['limit'] : 5;
			$startingID = isset($_GET['startingID']) ? $_GET['startingID'] : PHP_INT_MAX;
			$userID = $_GET['userID'];

			$gameIDArrays = $this->selectMultiple("SELECT gameID FROM GameUser WHERE userID = $userID and gameID < $startingID ORDER BY gameID DESC LIMIT $limit");
			$gameArrays = array();

			// Get each game's info
			foreach ($gameIDArrays as $gameIDArray) {
				$gameID = $gameIDArray['gameID'];
				$gameArray = $this->select("SELECT * FROM Game WHERE gameID = $gameID");

				// Get information about users
				$gameArray['users'] = $this->selectMultiple("SELECT userID, errorLogName, rank FROM GameUser WHERE gameID = $gameID");
				foreach($gameArray['users'] as &$gameUserRow) {
					// Get rid of gameID
					unset($gameUserRow['gameID']);

					// Add in user info
					$userInfo = $this->select("SELECT username FROM User WHERE userID = {$gameUserRow['userID']}");
					foreach($userInfo as $key => $value) $gameUserRow[$key] = $value;
				}
				array_push($gameArrays, $gameArray);
			}
			return $gameArrays;
		} else if(isset($_GET['random'])) {
			return $this->select("SELECT replayName FROM (SELECT * FROM Game ORDER BY gameID DESC LIMIT 50) sub ORDER BY rand() LIMIT 1");

		}
	}
	
	/* Bot File Endpoint
	 *
	 * Handles the user's submission of a new bot
	 */
	protected function botFile() {
		// Mark a new botfile for compilation if valid. Return error otherwise 
		if(isset($_FILES['botFile']['name']) && isset($_POST['userID']) && isset($_POST['password'])) {
			if($this->testUserIP($_SERVER['REMOTE_ADDR'], $this->TS_CDIRS) && !in_array($_SERVER['REMOTE_ADDR'], $this->TS_WIFI_IPS)) {
				return "Cannot submit on Two Sigma desktop";
			}

			$userID = $_POST['userID'];
			$password = $_POST['password'];

			$user = $this->select("SELECT * FROM User WHERE userID={$userID} and password='{$password}'");
			if(count($user) == 0 || $user['isVerified'] == false) {
				return "Unverified email";
			}

			
			if($user['status'] == 1 || $user['status'] == 2) {
				return "Compiling";
			}
			
			if ($_FILES["botFile"]["size"] > 20000000) {
				return "Sorry, your file is too large.";
			}

			$targetPath = BOTS_PATH."{$userID}.zip";
			if(file_exists($targetPath))  {
				unlink($targetPath);	
			}

			if(!move_uploaded_file($_FILES['botFile']['tmp_name'], $targetPath)) {
				return null;
			}

			$oldGameUsers = $this->selectMultiple("SELECT gameID FROM GameUser WHERE userID=$userID");
			foreach($oldGameUsers as $oldGameUser) {
				$this->insert("DELETE FROM Game WHERE gameID={$oldGameUser['gameID']}");
				$this->insert("DELETE FROM GameUser WHERE gameID={$oldGameUser['gameID']}");
			}
			
			$numPlayers = mysqli_query($this->mysqli, "SELECT userID FROM User WHERE status=3")->num_rows;
			$this->insert("INSERT INTO UserHistory (userID, versionNumber, lastRank, lastNumPlayers, lastNumGames) VALUES ($userID, {$user['numSubmissions']}, {$user['rank']}, $numPlayers, {$user['numGames']})");

			$this->insert("UPDATE User SET numSubmissions=numSubmissions+1, numGames=0, status = 1, mu = 25.000, sigma = 8.333 WHERE userID = $userID");

			return "Success";
		}
	}
	
	/* Forums Endpoint
	 *
	 * Handle the Discourse forums (forums.halite.io) single sign on authentication.
	 */
	protected function forums() {
		// Follows the Discource sso detailed here: https://meta.discourse.org/t/official-single-sign-on-for-discourse/13045
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
	
	/* Worker Endpoint
	 *
	 * Bots are compiled and run in games by a decentralized network of 'worker' servers.
	 * A few stats about each worker are recorded by our 'manager' server.
	 * These stats are exposed on our status page.
	 */
	protected function worker() {
		$workers = $this->selectMultiple("SELECT * FROM Worker ORDER BY apiKey");
		return $workers;
	}
	
	/* Stats endpoint
	 *
	 * Provides a number of statistics about the competition,
	 * which would be expensive or impossible to determine using our generic endpoints.
	 */ 
	protected function stats() {
		if(isset($_GET['throughput'])) {
			return mysqli_query($this->mysqli, "SELECT * FROM Game WHERE TIMESTAMPDIFF(DAY, timestamp, NOW()) < 1")->num_rows;
		}

		// Get the number of active users
		else if(isset($_GET['numSubmissions'])) {
			return $this->select("SELECT SUM(numSubmissions) FROM User")["SUM(numSubmissions)"];
		}

		// Get the number of active users
		else if(isset($_GET['numActive'])) {
			return mysqli_query($this->mysqli, "SELECT userID FROM User WHERE status = 3")->num_rows;
		} 
	}

	/* Announcement Endpoint
	 *
	 * Annoucements are used as 'news blasts' without requiring email.
	 * An alert with the annoucement's body and header is showed to users.
	 * Once it has been closed, it is never shown to that user again
	 */
	protected function announcement() {
		// Get the newest annoucement available to a user
		if(isset($_GET['userID'])) {
			return $this->select("SELECT a.* FROM Announcement a WHERE NOT EXISTS(SELECT NULL FROM DoneWithAnnouncement d WHERE d.userID = {$_GET['userID']} and d.announcementID = a.announcementID) ORDER BY announcementID LIMIT 1;");
		} 
		
		// Mark an annoucement as closed	
		else if(isset($_POST['announcementID']) && isset($_POST['userID']) && isset($_POST['password'])) {
			$announcementID = $_POST['announcementID'];
			$userID = $_POST['userID'];
			$password = $_POST['password'];

			if(count($this->select("SELECT * FROM User WHERE userID=$userID and password='$password' LIMIT 1")) > 0) {
				$this->insert("INSERT INTO DoneWithAnnouncement (userID, announcementID) VALUES ($userID, $announcementID)");
				return "Success";
			}
			return "Fail";
		}
	}
	
	/* Error Log Endpoint
	 *
	 * When a users times-out or errors during a game of Halite on our server,
	 * we store their stdout and stderr output and make it available to them.
	 * Users may only see **their** error logs.
	 */
	protected function errorLog() {
		session_start();

		// Return the requested error log only if it belongs to the signed in user.
		if(isset($_GET['errorLogName']) && count($this->select("SELECT * FROM GameUser WHERE errorLogName='{$_GET['errorLogName']}' and userID={$_SESSION['userID']}"))) {
			$targetPath = ERRORS_PATH."{$_GET['errorLogName']}"; 
			if(file_exists($targetPath) == false) {
				return null;
			}

			header($_SERVER["SERVER_PROTOCOL"] . " 200 OK");
			header("Cache-Control: public"); // needed for internet explorer
			header("Content-Type: application/txt");
			header("Content-Transfer-Encoding: Binary");
			header("Content-Length:".filesize($targetPath));
			header("Content-Disposition: attachment; filename=error.log");
			readfile($targetPath);
			die();
		}
	}
	
	/* Session Endpoint
	 *
	 * Encapsulates the logged in user's info
	 */
	protected function session() {
		session_set_cookie_params(7*24*3600);
		session_start();

		// Get the logged in user's info
		if($this->method == 'GET') {
			if(count($_SESSION) > 0) return $_SESSION;
			else return NULL;
		} 
		
		// Login a new user with a username and a password 
		// TODO: take this out, functionality can be achieved by asking for its userID
		else if(isset($_POST['username']) & isset($_POST['password'])) {
			$username = $_POST['username'];
			$password = $this->encryptPassword($_POST['password']);

			$user = $this->select("SELECT * FROM User WHERE username = '$username' AND password = '$password'");
			if($user['isVerified'] == false) {
				return "Unverified user";
			}
			$_SESSION = $user;
			return "Success";
		} 
		
		// Login a new user with a userID and a password
		else if(isset($_POST['userID']) & isset($_POST['password'])) {
			$userID = $_POST['userID'];
			$password = $this->encryptPassword($_POST['password']);

			$user = $this->select("SELECT * FROM User WHERE userID = $userID AND password = '$password'");
			if($user['isVerified'] == false) {
				return "Unverified user";
			}
			$_SESSION = $user;
			return "Success";
		} 
		
		// Log out a user
		else if($this->method == 'DELETE') {
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
