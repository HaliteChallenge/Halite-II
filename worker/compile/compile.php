<?php
	function isDirEmpty($dir) {
		if (!is_readable($dir)) return NULL;

		$handle = opendir($dir);
		while (false !== ($entry = readdir($handle))) {
			if ($entry != "." && $entry != "..") {
				return FALSE;
			}
		}
		return TRUE;
	}

	if(!isset($_GET['userID'])) {
		echo "Incorrect HTTP parameters.";
		exit(0);
	}

	$userID = $_GET['userID'];
	
	$storageDir = "../../storage/bots/$userID";
	$tempDir = "$userID";

	if(!file_exists($storageDir) || isDirEmpty($storageDir)) {
		echo "Necessary files do not exist";
		exit(0);
	}
	
	exec("mkdir $tempDir");
	exec("cp -a $storageDir/. $tempDir");
	
	exec("chmod 777 $tempDir");
	exec("python compiler.py $tempDir");

	if(file_exists("{$tempDir}/run.sh")) {
		exec("cp {$tempDir}/run.sh $storageDir");

		echo "Success";
	} else {
		echo "Cannot compile this bot";
	}

	exec("rm -r $tempDir");
?>
