<?php

include_once "../website/php/WebsiteAPI.php";
include_once "APITest.php";
include_once "UserTest.php";

class BotFileTests extends APITest { 
	public function testGET() {
		$this->insertObject(USER_TABLE, TEST_USER);
		$hashedPassword = $this->mysqli->query("SELECT password FROM User WHERE userID=".TEST_USER['userID'])->fetch_assoc()['password'];
		$_POST['userID'] = TEST_USER_HISTORY['userID'];
		$_POST['password'] = $hashedPassword;
		$_SERVER['REQUEST_METHOD'] = "POST";

    	$returnedGames = json_decode((new WebsiteAPI("botFile"))->processAPI());
		
		$idealUser = TEST_GAME_USER;
		$idealUser["username"] = TEST_USER['username'];

		$idealGame = TEST_GAME;
		$idealGame["users"] = array($idealUser);
		$this->assertEquals($returnedGames[0], $idealGame);
	}
}

?>
