<?php
include_once "../website/php/WebsiteAPI.php";

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
		$tables = array();
		$res = $this->mysqli->query("SELECT TABLE_NAME FROM information_schema.tables where TABLE_SCHEMA='Halite'");
		while($row = $res->fetch_assoc()) array_push($tables, $row['TABLE_NAME']);

		foreach($tables as $table) {
			$this->mysqli->query("DELETE FROM ".$table);
		}
	}

	protected function insertObject($table, $obj) {
		$sql = "INSERT INTO $table (".implode(",", array_keys($obj)).") VALUES ('".implode("','", array_values($obj))."')";
		echo $sql."\n";
		$this->mysqli->query($sql);
	}
}

?>
