<?php
include "../website/php/WebsiteAPI.php";

class APITest extends PHPUnit_Framework_TestCase {
	protected $mysqli;
	protected $config;

	protected function setUp() {
		$this->config = parse_ini_file("../halite.ini", true);
		$this->mysqli = new mysqli($this->config['database']['hostname'],
			$this->config['database']['username'],
			$this->config['database']['password'],
			$this->config['database']['name']);

		if (mysqli_connect_errno()) {
			echo "<br><br>There seems to be a problem with our database. Reload the page or try again later.";
			exit();
		}
	}

	protected function tearDown() {
		$tables = array("User", "UserHistory");
		foreach($tables as $table) {
			$this->mysqli->query("DELETE FROM ".$table);
		}
	}

	protected function insertObject($table, $obj) {
		$this->mysqli->query("INSERT INTO $table (".implode(",", array_keys($obj)).") VALUES ('".implode("','", array_values($obj))."')");
	}
}

define("USER_TABLE", "User");
const TEST_USER = array("username" => "testUsername", "password" => "testedUnhashedPassword", "email" => "testEmail", "verificationCode" => "1234567");
const TEST_USER_HISTORY = array("userID" => "123", "lastNumGames" => "100", "lastNumPlayers" => "20", "versionNumber" => "1", "lastRank" => "2");
const TEST_EXTRA_STATS = array("userID" => "123", "territoryRank" => "2");

class UserTest extends APITest { 
	public function testGetUser() {
		$this->insertObject(USER_TABLE, TEST_USER);
		$id = $this->mysqli->insert_id; 
		
		// Get with ID			
		$_GET['userID'] = $id;
		$_SERVER['REQUEST_METHOD'] = "GET";
    	$returnedUser = json_decode((new WebsiteAPI("user"))->processAPI());

		$this->assertObjectNotHasAttribute("email", $returnedUser);
		$this->assertObjectNotHasAttribute("verificationCode", $returnedUser);
		$this->assertObjectNotHasAttribute("password", $returnedUser);
		$this->assertEquals($returnedUser->username, TEST_USER['username']);
		
		// Get with username			
		unset($_GET['userID']);
		$_GET['username'] = TEST_USER['username'];
    	$returnedUser = json_decode((new WebsiteAPI("user"))->processAPI());

		$this->assertObjectNotHasAttribute("email", $returnedUser);
		$this->assertObjectNotHasAttribute("verificationCode", $returnedUser);
		$this->assertObjectNotHasAttribute("password", $returnedUser);
		$this->assertEquals($returnedUser->username, TEST_USER['username']);
	}

	public function testActive() {
		$inactiveUser = TEST_USER;
		$this->insertObject(USER_TABLE, $inactiveUser);

		$activeUser = TEST_USER; 
		$activeUser["username"] = "ACTIVE USERNAME";
		$this->insertObject(USER_TABLE, $activeUser);
		$activeID = $this->mysqli->insert_id; 
		$this->mysqli->query("UPDATE User SET status=3 where userID=$activeID");
		
		$_GET['active'] = 1;
		$_SERVER['REQUEST_METHOD'] = "GET";
    	$returnedUsers = json_decode((new WebsiteAPI("user"))->processAPI());
		
		$this->assertEquals(count($returnedUsers), 1);	

		$returnedActiveUser = $returnedUsers[0];
		$this->assertObjectNotHasAttribute("email", $returnedActiveUser);
		$this->assertObjectNotHasAttribute("verificationCode", $returnedActiveUser);
		$this->assertObjectNotHasAttribute("password", $returnedActiveUser);

		$this->assertEquals($returnedActiveUser->username, $activeUser['username']);
	}

	public function testVerify() {
		$this->insertObject(USER_TABLE, TEST_USER);
		$id = $this->mysqli->insert_id; 
		
		$_POST['userID'] = $id;
		$_POST['verificationCode'] = TEST_USER['verificationCode'];
		$_SERVER['REQUEST_METHOD'] = "POST";
    	(new WebsiteAPI("user"))->processAPI();

		$newUser = $this->mysqli->query("SELECT * FROM User WHERE userID=$id")->fetch_assoc();

		$this->assertEquals(intval($newUser["isVerified"]), 1);
	}
}

define("EXTRA_STATS_TABLE", "UserExtraStats");
class ExtraStatsTests extends APITest { 
	public function testGET() {
		$this->insertObject(EXTRA_STATS_TABLE, TEST_EXTRA_STATS);
		
		$_POST['userID'] = TEST_EXTRA_STATS['userID'];
		$_SERVER['REQUEST_METHOD'] = "POST";

    	$returnedStats = json_decode((new WebsiteAPI("extraStats"))->processAPI());

		$this->assertEquals($returnedStats, TEST_EXTRA_STATS);
	}
}

define("HISTORY_TABLE", "UserHistory");
class HistoryTests extends APITest { 
	public function testGET() {
		$this->insertObject(HISTORY_TABLE, TEST_USER_HISTORY);
		
		$_POST['userID'] = TEST_USER_HISTORY['userID'];
		$_SERVER['REQUEST_METHOD'] = "POST";

    	$returnedHistory = json_decode((new WebsiteAPI("history"))->processAPI());

		$this->assertEquals($returnedHistory, TEST_USER_HISTORY);
	}
}
?>
