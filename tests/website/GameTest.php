<?php

include_once "../website/php/WebsiteAPI.php";
include_once "APITest.php";
include_once "UserTest.php";

define("GAME_TABLE", "Game");
define("GAME_USER_TABLE", "User");

const TEST_GAME = array("gameID" => "1234", "replayName" => "sdkfjlad.hlt");
const TEST_GAME_USER = array("gameID" => "1234", "errorLogName" => "asdfkjl.log", "rank" => "1", "userID" => TEST_USER['userID']);
class GameTest extends APITest { 
	public function test_game() {
		echo "MADE IT";
		$this->insertObject(GAME_TABLE, TEST_GAME);
		$this->insertObject(GAME_USER_TABLE, TEST_GAME_USER);
		$this->insertObject(USER_TABLE, TEST_USER);
		
		$_GET['userID'] = TEST_USER_HISTORY['userID'];
		$_SERVER['REQUEST_METHOD'] = "GET";

    	$returnedGames = json_decode((new WebsiteAPI("game"))->processAPI());
		
		$idealUser = TEST_GAME_USER;
		$idealUser["username"] = TEST_USER['password'];

		$idealGame = TEST_GAME;
		$idealGame["users"] = array($idealUser);
		$this->assertEquals($returnedGames[0], $idealGame);
	}
}
?>
