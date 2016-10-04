<?php

include_once "../website/api/web/WebsiteAPI.php";
include_once "APITest.php";
include_once "UserTest.php";

define("GAME_TABLE", "Game");
define("GAME_USER_TABLE", "GameUser");

const TEST_GAME = array("gameID" => "1234", "replayName" => "sdkfjlad.hlt", "mapWidth" => "50", "mapHeight" => "50");
const TEST_GAME_USER = array("gameID" => "1234", "errorLogName" => "asdfkjl.log", "rank" => "1", "userID" => TEST_USER['userID']);
class GameTest extends APITest {
    public function testGET() {
        echo "MADE IT";
        $this->insertObject(GAME_TABLE, TEST_GAME);
        $this->insertObject(GAME_USER_TABLE, TEST_GAME_USER);
        $this->insertObject(USER_TABLE, TEST_USER);

        $_GET['userID'] = TEST_USER['userID'];
        $_GET['startingID'] = 100000;
        $_SERVER['REQUEST_METHOD'] = "GET";

        $returnedGame = json_decode((new WebsiteAPI("game"))->processAPI(), true)[0];

        $idealUser = TEST_GAME_USER;
        $idealUser["username"] = TEST_USER['username'];
        unset($idealUser["gameID"]);

        $idealGame = TEST_GAME;
        $idealGame["users"] = array($idealUser);

        $this->assertArrayHasKey("timestamp", $returnedGame);
        $this->assertArraySubset($idealGame, $returnedGame);
    }
}
?>
