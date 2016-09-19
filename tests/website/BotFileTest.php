<?php

include_once "../website/php/WebsiteAPI.php";
include_once "APITest.php";
include_once "UserTest.php";

class BotFileTests extends APITest { 
	public function testPOST() {
		$testUser = TEST_USER;
		$testUser['rank'] = 1;
		$testUser['isRunning'] = 1;
		$this->insertObject(USER_TABLE, $testUser);

		$botPath = COMPILE_PATH.$testUser['userID'].".zip";
		if(file_exists($botPath)) unlink($botPath);
		
		$_FILES = array(
			'botFile' => array(
				'name' => 'testFile.txt',
				'type' => 'text',
				'size' => 542,
				'tmp_name' => __DIR__ . '/testFile.txt',
				'error' => 0
			)
        );
		$_SERVER['REMOTE_ADDR'] = "127.0.0.1";
		$_SERVER['REQUEST_METHOD'] = "POST";

		$this->assertEquals((new WebsiteAPI("botFile"))->processAPI(), "null");	

		$_SESSION['userID'] = $testUser['userID'];
    	$result = (new WebsiteAPI("botFile"))->processAPI();

		$newUser = $this->mysqli->query("SELECT * FROM User WHERE userID={$testUser['userID']}")->fetch_assoc();
		var_dump($newUser);
		$userHistory = $this->mysqli->query("SELECT * FROM UserHistory WHERE userID={$testUser['userID']}")->fetch_assoc();

		$this->assertEquals($newUser["compileStatus"], "1");
		$this->assertEquals($newUser["isRunning"], "1");
		$this->assertEquals(count($userHistory), 0);
		$this->assertTrue(file_exists($botPath));
	}
}

?>
