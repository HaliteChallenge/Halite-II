<?php

include_once "../website/php/WebsiteAPI.php";
include_once "APITest.php";
include_once "UserTest.php";

class BotFileTests extends APITest { 
	public function testPOST() {
		$this->insertObject(USER_TABLE, TEST_USER);
		$hashedPassword = $this->mysqli->query("SELECT password FROM User WHERE userID=".TEST_USER['userID'])->fetch_assoc()['password'];
		$_POST['userID'] = TEST_USER_HISTORY['userID'];
		$_POST['password'] = $hashedPassword;
		$_SERVER['REQUEST_METHOD'] = "POST";

    	(new WebsiteAPI("botFile"))->processAPI();

		$newUser = $this->mysqli->query("SELECT * FROM User WHERE userID=".TEST_USER['userID'])->fetch_assoc();
		$userHistory = $this->mysqli->query("SELECT * FROM UserHistory WHERE userID=".TEST_USER['userID'])->fetch_assoc();
	}
}

?>
