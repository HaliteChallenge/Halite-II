<?php
include_once "../website/php/WebsiteAPI.php";
include_once "APITest.php";
include_once "UserTest.php";

define("HISTORY_TABLE", "UserHistory");
const TEST_USER_HISTORY = array("userID" => "123", "lastNumGames" => "100", "lastNumPlayers" => "20", "versionNumber" => "1", "lastRank" => "2");
class HistoryTest extends APITest { 
	public function testGET() {
		$this->insertObject(HISTORY_TABLE, TEST_USER_HISTORY);
		
		$_GET['userID'] = TEST_USER_HISTORY['userID'];
		$_GET['REQUEST_METHOD'] = "POST";

    	$returnedHistory = json_decode((new WebsiteAPI("history"))->processAPI());

		$this->assertEquals($returnedHistory, TEST_USER_HISTORY);
	}
}
?>
