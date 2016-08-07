<?php

require_once 'API.class.php';

define("REPLAYS_DIR", "../storage/replays/");
define("ERROR_LOGS_DIR", "../storage/errors/");
define("BOTS_DIR", "../storage/bots/");
define("CACHED_BOTS_DIR", "../storage/cache/");
define("INI_FILE", "../halite.ini");

class ManagerAPI extends API{
		private $mysqli = NULL;
		private $apiKey = NULL;

		// Init database, sanitize parameters, and check if worker is valid
		public function __construct($request, $origin) {
				$this->initDB();

				if($this->isValidWorker() == false) {
						echo "Not valid worker";
						exit(1);
				} else {
						$this->insert("UPDATE Worker SET lastRequestTime = now() WHERE apiKey = {$this->apiKey}");
				}
				parent::__construct($request);
		}

		private function getAPIKey() {
				$this->apiKey = NULL;
				if(isset($_GET['apiKey'])) $this->apiKey = $_GET['apiKey'];
				else if(isset($_POST['apiKey'])) $this->apiKey = $_POST['apiKey'];
		}

		// Checks HTTP parameters and request IP to make sure that the client provided a valid API key
		private function isValidWorker() {
				// Get apiKey
				$this->getAPIKey();
				if($this->apiKey == NULL) return false;

				// Get ip
				$ipAddress = $_SERVER['REMOTE_ADDR'];

				if(count($this->select("SELECT ipAddress FROM Worker WHERE ipAddress = '".$this->mysqli->real_escape_string($ipAddress)."' and apiKey = ".$this->mysqli->real_escape_string($this->apiKey))) > 0) {
						return true;
				} else {
						return false;
				}
		}

		// Returns the directory that holds a bot, given the bot's userID
		private function getBotFile($userID) {
				return BOTS_DIR."{$userID}.zip";
		}

		private function getTrueskillMatchQuality($rankingValues) {
				usort($rankingValues, function($a, $b) {
						return $a['rank'] < $b['rank'];
				});
				$rankings = array();
				foreach($rankingValues as $user) {
						array_push($rankings, $user['mu']);
						array_push($rankings, $user['sigma']);
				}
				exec("python3 trueskillMatchQuality.py ".implode(' ', $rankings), $lines);
				return floatval($lines[0]);
		}


		// Initializes and returns a mysqli object that represents our mysql database
		private function initDB() {
				$config = parse_ini_file(INI_FILE, true);
				$this->mysqli = new mysqli($config['database']['hostname'],
						$config['database']['username'],
						$config['database']['password'],
						$config['database']['name']);

				if (mysqli_connect_errno()) {
						echo "<br><br>There seems to be a problem with our database. Reload the page or try again later.";
						exit();
				}
		}

		private function select($sql) {
				try {
						$res = mysqli_query($this->mysqli, $sql);
						$array = mysqli_fetch_array($res, MYSQLI_ASSOC);
						return $array;
				} catch(Exception $e) {
						return array();
				}
		}

		private function selectMultiple($sql) {
				$res = mysqli_query($this->mysqli, $sql);
				$finalArray = array();

				while($temp = mysqli_fetch_array($res, MYSQLI_ASSOC)) {
						array_push($finalArray, $temp);
				}

				return $finalArray;
		}

		private function insert($sql) {
				mysqli_query($this->mysqli, $sql);
		}

		/////////////////////////API ENDPOINTS\\\\\\\\\\\\\\\\\\\\\\\\\\\\

		// Delegate a task to a workers
		protected function task() {
				if($this->method == 'GET') {
						// Check for compile tasks
						$needToBeCompiled = $this->select("SELECT * FROM User WHERE status = 1 ORDER BY userID ASC");
						if(count($needToBeCompiled) > 0) {
								$this->insert("UPDATE User SET status = 2 WHERE userID = ".$needToBeCompiled['userID']);
								return array(
										"type" => "compile",
										"user" => $needToBeCompiled);
						}

						// Assign a run game tasks
						$possibleNumPlayers = array(2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6);
						$numPlayers = $possibleNumPlayers[array_rand($possibleNumPlayers)];

						$users = $this->selectMultiple("SELECT * FROM User WHERE status=3 ORDER BY rand()");
						//$seedPlayer = $this->select("select * from User where status=3 and (select COUNT(*) from GameUser where userID=User.userID) < 100 ORDER BY rand() LIMIT 1");
						$seedPlayer = $this->select("select * from User inner join UserExtraStats on User.userID=UserExtraStats.userID where status=3 and (numGames < 40 or didTimeout < 0.9) order by rand()*-sigma");
						if(count($seedPlayer) < 1) {
							$oldestGameTime = time();
							foreach($users as $user) {
								$latestGameTime = strtotime($this->select("select timestamp from Game inner join GameUser on Game.gameID = GameUser.gameID and GameUser.userID={$user['userID']} order by timestamp DESC limit 1")['timestamp']);
								if($latestGameTime < $oldestGameTime) {
									$seedPlayer = $user;
									$oldestGameTime = $latestGameTime;
								}
							}
						}
						$players = $this->selectMultiple("SELECT * FROM User WHERE status=3 and ABS(rank-{$seedPlayer['rank']}) < (5 / pow(rand(), 0.65)) and userID <> {$seedPlayer['userID']} ORDER BY rand() LIMIT ".($numPlayers-1));
						array_push($players, $seedPlayer);

						// Pick map size
						$sizes = array(20, 25, 25, 30, 30, 30, 35, 35, 35, 35, 40, 40, 40, 45, 45, 50);
						$size = $sizes[array_rand($sizes)];

						// Send game task
						if(count($players) == $numPlayers) {
								return array(
										"type" => "game",
										"width" => $size,
										"height" => $size,
										"users" => $players
								);
						}
				}
		}

		// Allow worker to post the result of their compilation
		protected function compile() {
				var_dump($_POST);
				if(isset($_POST['userID']) && isset($_POST['didCompile'])) {
						$this->insert("UPDATE Worker SET numCompiles=numCompiles+1 WHERE apiKey=".$this->mysqli->real_escape_string($this->apiKey));

						$userID = $_POST['userID'];
						$didCompile = $_POST['didCompile'];

						if($didCompile == 1) {
								// Cache a compilable bot
								$targetPath = $this->getBotFile($userID);
								$cachedPath = CACHED_BOTS_DIR."{$userID}.zip";
								echo $cachedPath;
								if(file_exists($cachedPath)) {
										unlink($cachedPath);
								}
								copy($targetPath, $cachedPath);

								$language = isset($_POST['language']) ? $_POST['language'] : "Other";
								$this->insert("UPDATE User SET status = 3, language = '".$this->mysqli->real_escape_string($language)."' WHERE userID = ".$this->mysqli->real_escape_string($userID));
						} else {
								$versionNumber = intval($this->select("SELECT * FROM User WHERE userID=".$this->mysqli->real_escape_string($userID))['numSubmissions'])-1;
								$this->insert("DELETE FROM UserHistory WHERE userID=".$this->mysqli->real_escape_string($userID)." and versionNumber={$versionNumber}");

								$targetPath = $this->getBotFile($userID);
								$cachedPath = CACHED_BOTS_DIR."{$userID}.zip";
								echo $cachedPath;
								if(file_exists($cachedPath)) {
										unlink($targetPath);
										copy($cachedPath, $targetPath);
										$this->insert("UPDATE User SET status = 3, numSubmissions=numSubmissions-1 WHERE userID = ".$this->mysqli->real_escape_string($userID));
								} else {
										$this->insert("UPDATE User SET status = 0, numSubmissions=numSubmissions-1 WHERE userID = ".$this->mysqli->real_escape_string($userID));
								}
						}
				}
		}

		// Allow workers to post the result of their game
		protected function game() {
				var_dump($_FILES);
				var_dump($_POST);
				// Each user in users must have a rank, playerIndex, mu, sigma, and userID
				if(isset($_POST['users']) && count($_FILES) > 0) {
						$this->insert("UPDATE Worker SET numGames=numGames+1 WHERE apiKey=".$this->mysqli->real_escape_string($this->apiKey));

						$mapWidth = $_POST['mapWidth'];
						$mapHeight = $_POST['mapHeight'];
						$users = json_decode($_POST['users']);

						foreach($users as $user) {
							$storedUser = $this->select("SELECT status, numSubmissions FROM User WHERE userID=".$this->mysqli->real_escape_string($user->userID));
							if(intval($storedUser['numSubmissions']) != intval($user->numSubmissions)) {
								return null;
							}
						}

						// Store replay file and error logs
						$replayName = null;
						foreach($_FILES as $fileKey => $file) {
							$pathParts = pathinfo($file['name']);
							$targetPath = null;
							if(strcmp('hlt', $pathParts['extension']) == 0) {
								$replayName = $pathParts['basename'];
								$targetPath = REPLAYS_DIR."{$pathParts['basename']}";
							} else {
								$targetPath = ERROR_LOGS_DIR."{$pathParts['basename']}";
							}
							move_uploaded_file($file['tmp_name'], $targetPath);
							if(is_file($targetPath) == false) {
								return "Did not work";
							}
							chmod($targetPath, 0777);
						}

						// Check that we arent storing too many replay files
						$files = glob(REPLAYS_DIR.'*.*');
						$exclude_files = array('.', '..');
						if (!in_array($files, $exclude_files)) {
								array_multisort(
										array_map( 'filemtime', $files ),
										SORT_NUMERIC,
										SORT_ASC,
										$files
								);
						}
						$numAllowed = 40000;
						while(count($files) > $numAllowed) {
								unlink($files[0]);
								array_splice($files, 0, 1);
						}

						// Check that we arent stoing too many games in db
						$res = mysqli_query($this->mysqli, "SELECT * FROM Game");
						$numRows = $res->num_rows;
						$numToDelete = $numRows - $numAllowed;
						if($numToDelete > 0) {
								$gamesToDelete = $this->selectMultiple("SELECT gameID FROM Game ORDER BY gameID LIMIT $numToDelete");
								foreach($gamesToDelete as $game) {
										$this->insert("DELETE FROM GameUser WHERE gameID={$game['gameID']}");
										$this->insert("DELETE FROM Game WHERE gameID={$game['gameID']}");
								}
						}

						// Store game information in db
						$this->insert("INSERT INTO Game (replayName, mapWidth, mapHeight, timestamp) VALUES ('".$this->mysqli->real_escape_string($replayName)."', ".$this->mysqli->real_escape_string($mapWidth).", ".$this->mysqli->real_escape_string($mapHeight).", NOW())");
						$gameIDArray = $this->select("SELECT gameID FROM Game WHERE replayName = '".$this->mysqli->real_escape_string($replayName)."' LIMIT 1");
						$gameID = $gameIDArray['gameID'];

						// Update each participant's stats
						$allUsers = $this->selectMultiple("SELECT * FROM User where status=3");
						$allUserExtras = array();
						foreach($allUsers as $user) {
								array_push($allUserExtras, $this->select("SELECT * FROM UserExtraStats WHERE userID = {$user['userID']}"));
						}
						for($a = 0; $a < count($users); $a++) {
								$timeoutInt = $users[$a]->didTimeout ? 1 : 0;
								$errorLogName = $users[$a]->errorLogName == NULL ? "NULL" : "'".$this->mysqli->real_escape_string($users[$a]->errorLogName)."'";
								$this->insert("INSERT INTO GameUser (gameID, userID, errorLogName, rank, playerIndex, territoryAverage, strengthAverage, productionAverage, stillPercentage, turnTimeAverage, didTimeout) VALUES ($gameID, ".$this->mysqli->real_escape_string($users[$a]->userID).", $errorLogName, ".$this->mysqli->real_escape_string($users[$a]->rank).", ".$this->mysqli->real_escape_string($users[$a]->playerTag).", ".$this->mysqli->real_escape_string($users[$a]->territoryAverage).", ".$this->mysqli->real_escape_string($users[$a]->strengthAverage).", ".$this->mysqli->real_escape_string($users[$a]->productionAverage).", ".$this->mysqli->real_escape_string($users[$a]->stillPercentage).", ".$this->mysqli->real_escape_string($users[$a]->turnTimeAverage).", {$timeoutInt})");

								// Cache raw game stats
								$gameStats = $this->selectMultiple("SELECT territoryAverage, strengthAverage, productionAverage, stillPercentage, turnTimeAverage, didTimeout FROM GameUser WHERE userID=".$this->mysqli->real_escape_string($users[$a]->userID));
								$totalGameStats = array();
								foreach($gameStats as $oneGameStats) {
										foreach($oneGameStats as $statName => $statValue) {
												if(!array_key_exists($statName, $totalGameStats)) {
														$totalGameStats[$statName] = 0;
												}
												$totalGameStats[$statName] += $statValue;
										}
								}
								foreach($totalGameStats as $statName => $totalStatValue) {
										$averageStatValue = $totalStatValue / count($gameStats);
										$this->insert("UPDATE UserExtraStats SET $statName=$averageStatValue WHERE userID = ".$this->mysqli->real_escape_string($users[$a]->userID));
								}

								// Game game stat rankings
								$statToRankedStat = array("territoryAverage" => "territoryRanking", "strengthAverage" => "strengthRanking", "productionAverage" => "productionRanking", "stillPercentage" => "stillRanking", "turnTimeAverage" => "turnTimeRanking", "didTimeout" => "timeoutRanking");
								foreach($statToRankedStat as $statName => $rankedStatName) {
										usort($allUserExtras, function($a, $b) use ($statName) {
												return $a[$statName] < $b[$statName];
										});
										$rank = 100000;
										for($b = 0; $b < count($allUserExtras); $b++) {
												if($allUserExtras[$b]['userID'] == $users[$a]->userID) {
														$rank = $b+1;
														break;
												}
										}
										$this->insert("UPDATE UserExtraStats SET {$rankedStatName}={$rank} WHERE userID = ".$this->mysqli->real_escape_string($users[$a]->userID));
								}

								// Add to other stats
								$this->insert("UPDATE User SET numGames=numGames+1, mu = ".$this->mysqli->real_escape_string($users[$a]->mu).", sigma = ".$this->mysqli->real_escape_string($users[$a]->sigma)." WHERE userID = ".$this->mysqli->real_escape_string($users[$a]->userID));
						}

						// Update mu and sigma
						usort($users, function($a, $b) {
								return $a->rank > $b->rank;
						});
						$rankings = array();
						foreach($users as $user) {
								array_push($rankings, $user->mu);
								array_push($rankings, $user->sigma);
						}
						exec("python3 updateTrueskill.py ".implode(' ', $rankings), $lines);
						var_dump($lines);
						for($a = 0; $a < count($users); $a++) {
								$components = explode(' ', $lines[$a]);
								$this->insert("UPDATE User SET mu=".$this->mysqli->real_escape_string($components[0]).", sigma=".$this->mysqli->real_escape_string($components[1])." WHERE userID=".$this->mysqli->real_escape_string($users[$a]->userID));
						}

						// Update overall rank
						$allUsers = $this->selectMultiple("SELECT * FROM User where status=3");
						usort($allUsers, function($a, $b) {
								return $a['mu']-3*$a['sigma'] < $b['mu']-3*$b['sigma'];
						});
						for($userIndex = 0; $userIndex < count($allUsers); $userIndex++) {
								$rank = $userIndex+1;
								$this->insert("UPDATE User SET rank={$rank} WHERE userID={$allUsers[$userIndex]['userID']}");
						}

				}
		}

		// Allow workers to download and post bot files
		protected function botFile() {
				if(isset($_GET['userID'])) {
						$userID = $_GET['userID'];

						header("Content-disposition: attachment; filename={$userID}.zip");
						header("Content-type: application/zip");

						ob_clean();
						flush();
						readfile($this->getBotFile($userID));
						exit;
				} else if(isset($_POST['userID']) && count($_FILES) > 0) {
						$userID = $_POST['userID'];
						$key = array_keys($_FILES)[0];
						$name = basename($_FILES[$key]['name']);

						$targetPath = BOTS_DIR."{$userID}.zip";
						move_uploaded_file($_FILES[$key]['tmp_name'], $targetPath);
				} else {
						return NULL;
				}

				return "Success";
		}

		// Allow workers to get the hash of a bot file so that they know that they downloaded the file correctly
		protected function botHash() {
				if(isset($_GET['userID'])) {
						$userID = $_GET['userID'];
						if(file_exists($this->getBotFile($userID))) return array("hash" => md5_file($this->getBotFile($userID)));
						else return "Bot file does not exist";
				}
		}
}
?>
