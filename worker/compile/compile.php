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
	$dir = "../../storage/bots/$userID";

	if(!file_exists($dir) || isDirEmpty($dir)) {
		echo "Necessary files do not exist";
		exit(0);
	}
	var_dump(scandir($dir));
	
	echo "python compiler.py $dir<br>";
	shell_exec("python compiler.py $dir", $shellOutput);

	if(file_exists("{$dir}/run.sh")) {
		echo "Success";
	} else {
		echo "Cannot compile this bot";
	}
?>
