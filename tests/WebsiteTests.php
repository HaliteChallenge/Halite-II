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
		//$this->mysqli->query("DROP DATABASE ".$this->config['database']['name']);
	}

	protected function insertObject($obj) {
		$this->mysqli->query("INSERT INTO User (".implode(",", array_keys($obj)).") VALUES ('".implode("','", array_values($obj))."')");
	}
}

const TEST_USER = array("username" => "testUsername", "password" => "testedUnhashedPassword", "email" => "testEmail", "verificationCode" => "1234567");

class UserTest extends APITest { 
	public function testGetUser() {
		$this->insertObject(TEST_USER);
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
		$this->insertObject($inactiveUser);

		$activeUser = TEST_USER; 
		$activeUser["username"] = "ACTIVE USERNAME";
		$this->insertObject($activeUser);
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
		$this->insertObject(TEST_USER);
		$id = $this->mysqli->insert_id; 
		
		$_POST['userID'] = $id;
		$_POST['verificationCode'] = TEST_USER['verificationCode'];
		$_SERVER['REQUEST_METHOD'] = "POST";

		$newUser = $this->mysqli->query("SELECT * FROM User WHERE userID=$id")->fetch_assoc();

		$this->assertEquals(intval($newUser["isVerified"]), 1);
	}

}
?>
