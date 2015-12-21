<?php
	include "Compiler.php";

	class Sandbox {
		private $timeOut, $path, $folder, $vmName, $compilerName, $fileName, $code, $outputCommand, $languageName, $extraArguments;

		public function __construct($timeOut, $path, $folder, $vmName, $compilerName, $fileName, $code, $outputCommand, $languageName, $extraArguments) {
			$this->timeOut = $timeOut;
			$this->path = $path;
			$this->folder = $folder;
			$this->vmName = $vmName;
			$this->compilerName = $compilerName;
			$this->fileName = $fileName;
			$this->code = $code;
			$this->outputCommand = $outputCommand;
			$this->languageName = $languageName;
			$this->extraArguments = $extraArguments;
		}

		public function languages() {
			return array(
			    "C/C++" => [0, "text/x-c++src"],
			    "Java" => [1, "text/x-java"],
			    "Python" => [2, "text/x-python"]
			);
		}

		public function compilers() {
			return array(
				new Compiler("python", "file.py", "", "Python", ""),
				new Compiler("\'g++ -o /usercode/a.out\' ", "file.cpp", "/usercode/a.out", "C/C++", ""),
				new Compiler("javac", "file.java", "\'./usercode/javaRunner.sh\'", "Java", "")
			);
		}

		// Runs the Sandbox. Callback function is given the output of the code and the errors the code generates in that order.
		public function run($callback) {
			$sandbox = $this;
			$this->prepare(function() {
				$sandbox->execute($callback);
			});
		}

		/*
		 * Function that creates a directory with the folder name already provided through constructor
         * and then copies contents of folder named Payload to the created folder, this newly created folder will be mounted
         * on the Docker Container. A file with the name specified in file_name variable of this class is created and all the
         * code written in 'code' variable of this class is copied into this file.
		*/
		public function prepare($callback) {
			// Make a folder specified by the folder member variable
			// Copy the Payload folder
			// Allow all users to edit the new folder
			exec("mkdir $this->path$this->folder && cp $this->path/Payload/* $this->path$this.folder  && chmod 777 $this->path$this->folder");
			
			// Write code to file in Sandbox
			$codeFile = fopen("$this->path{$this->folder}/$this->fileName", "w");
			fwrite($codeFile, $this->code);
			fclose($codeFile);
			
			exec("chmod 777 \'{$this->path}{$this->folder}/{$this->fileName}\'");

			$callback();
		}
		/*
		 * This function takes the newly created folder prepared by Sandbox.prepare() and spawns a Docker container
         * with the folder mounted inside the container with the name '/usercode/' and calls the script.sh file present in that folder
         * to carry out the compilation. The Sandbox times out at timeOut seconds.
         * In the end the function deletes the temporary folder and the docker container
		*/
		public function execute($callback) {
			$statement = "timeout {$timeOut} {$this->path}DockerTimeout.sh {$this->timeOut}s -u mysql -e -i -t -v \"{$this->path}{$this->folder}\":/usercode {$this->vmName} /usercode/script.sh {$this->compilerName} {$this->fileName} {$this->outputCommand} {$this->extraArguments}";
			
			echo $statement;

			// Execute Docker container
			exec($statement);

			// Read in the output file
			$outputContents = file_get_contents("{$this->path}{$this->folder}/completed");
			$errorContents = file_get_contents("{$this->path}{$this->folder}/errors");

			// If file reading failed, set to null
			if($outputContents == FALSE) $outputContents = NULL;
			if($errorContents == FALSE) $errorContents = NULL;

			// Delete the temporary directory and effectively, the container
			exec("rm -r {$this->folder}");

			$callback($outputContents, $errorContents);

		}
	}
?>