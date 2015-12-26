<?php

require_once '../API.class.php';
class ManagerAPI extends API
{

	// The database
	private $mysqli = NULL;

	public function __construct($request, $origin) {
		$this->initDB();

		// Sanitize HTTP parameters
		foreach ($_GET as $key => $value) {
			$_GET[$key] = $this->mysqli->real_escape_string($value);
		}

		parent::__construct($request);
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
 }

 ?>