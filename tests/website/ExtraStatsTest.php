<?php

include_once "../website/php/WebsiteAPI.php";
include_once "APITest.php";
include_once "UserTest.php";

define("EXTRA_STATS_TABLE", "UserExtraStats");
const TEST_EXTRA_STATS = array("userID" => "123", "territoryRanking" => "2");
class ExtraStatsTest extends APITest { 
	public function testGET() {
		$this->insertObject(EXTRA_STATS_TABLE, TEST_EXTRA_STATS);
		
		$_GET['userID'] = TEST_EXTRA_STATS['userID'];
		$_SERVER['REQUEST_METHOD'] = "GET";

    	$returnedStats = json_decode((new WebsiteAPI("extraStats"))->processAPI(), true);

		$this->assertArraySubset(TEST_EXTRA_STATS, $returnedStats);
	}
}
?>
