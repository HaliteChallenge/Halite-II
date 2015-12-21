<?php

final class Compiler {
	// The compiler command
	public $compilerName;
	// The name of the file that the compiler will compile
	public $fileName;
	// The command that outputs the compiled code to a binary
	public $outputCommand;
	// The name of the language that this compiler compiles
	public $languageName;
	// Any extra arguments
	public $extraArguments; 

	public function __construct($compilerName, $fileName, $outputCommand, $languageName, $extraArguments) {
		$this->compilerName = $compilerName;
		$this->fileName = $fileName;
		$this->outputCommand = $outputCommand;
		$this->languageName = $languageName;
		$this->extraArguments = $extraArguments;
	}
}

?>