<?php

include_once "../website/php/WebsiteAPI.php";
include_once "APITest.php";
include_once "UserTest.php";

class BotFileTests extends APITest { 
	public function testPOST() {
		$testUser = TEST_USER;
		$testUser['isVerified'] = 1;
		$testUser['rank'] = 1;
		$testUser['status'] = 3;
		$this->insertObject(USER_TABLE, $testUser);

		$botPath = BOTS_PATH.$testUser['userID'].".zip";
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
		$_POST['userID'] = $testUser['userID'];
		$_POST['password'] = $testUser['password'];
		$_SERVER['REMOTE_ADDR'] = "127.0.0.1";
		$_SERVER['REQUEST_METHOD'] = "POST";

    	$result = (new WebsiteAPI("botFile"))->processAPI();

		$newUser = $this->mysqli->query("SELECT * FROM User WHERE userID={$testUser['userID']}")->fetch_assoc();
		$userHistory = $this->mysqli->query("SELECT * FROM UserHistory WHERE userID={$testUser['userID']}")->fetch_assoc();

		$this->assertEquals($newUser["status"], "1");
		$this->assertTrue(count($userHistory) > 0);
		$this->assertTrue(file_exists($botPath));
	}
}

?>
