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
		$this->mysqli->query("DROP DATABASE ".$this->config['database']['name']);
	}

	protected function insertObject($obj) {
		var_dump($this->mysqli->query("INSERT INTO User (".implode(",", array_keys($obj)).") VALUES ('".implode("','", array_values($obj))."')"));
	}
}

const TEST_USER = array("username" => "testUsername", "password" => "testedUnhashedPassword", "email" => "testEmail", "verificationCode" => "1234567");

class UserTest extends APITest { 
	public function testGET() {
		$this->insertObject(TEST_USER);
		$id = $this->mysqli->insert_id; 
		
		$_GET['userID'] = $id;
		$_SERVER['REQUEST_METHOD'] = "GET";
    	$returnedUser = json_decode((new WebsiteAPI("user"))->processAPI());
		var_dump($returnedUser);

		$this->assertObjectNotHasAttribute("email", $returnedUser);
		$this->assertObjectNotHasAttribute("verificationCode", $returnedUser);
		$this->assertObjectNotHasAttribute("password", $returnedUser);

		$this->assertEquals($returnedUser->username, TEST_USER['username']);
	}
}
?>
