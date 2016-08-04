<?php
include "../website/php/WebsiteAPI.php";

class APITest extends PHPUnit_Framework_TestCase {
	private $mysqli;
	private $config;

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

	protected function insertObject($obj) {
		$this->mysqli->query("INSERT INTO User ('".implode("','", array_keys($obj))."') VALUES ('".implode("','", array_values($obj))."')");	
	}
}

const TEST_USER = array("username" => "testUsername", "password" => "testedUnhashedPassword", "email" => "testEmail", "verificationCod" => "1234567");

class UserTest extends APITest { 
	public function testGET() {
		$this->insertObject(TEST_USER);
		echo "HII";
		var_dump($this->mysqli->query("SELECT * FROM USER")->fetch_assoc);
	}
}
?>
