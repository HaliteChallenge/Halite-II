<?php

use Aws\Sdk;
require __DIR__ . '/../../vendor/autoload.php';

require_once '../API.class.php';

define("COMPILE_PATH", "../../../storage/compile/");
define("BOTS_PATH", "../../../storage/bots/");
define("INI_FILE", "../../../halite.ini");

ini_set('upload_max_filesize', '50M');
ini_set('post_max_size', '50M');
ini_set('max_input_time', 90);
ini_set('max_execution_time', 90);
ini_set('display_errors', 1);
error_reporting(E_ALL ^ E_NOTICE);

class ManagerAPI extends API{
    private $apiKey = NULL;

    // Init database, sanitize parameters, and check if worker is valid
    public function __construct($request) {
        $this->loadConfig();
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


    /////////////////////////API ENDPOINTS\\\\\\\\\\\\\\\\\\\\\\\\\\\\

    // Delegate a task to a workers
    protected function task() {
        if($this->method == 'GET') {
            // Check for compile tasks
            $needToBeCompiled = $this->select("SELECT * FROM User WHERE compileStatus=1 ORDER BY userID ASC");
            if(count($needToBeCompiled) > 0) {
                $this->insert("UPDATE User SET compileStatus = 2 WHERE userID = ".$needToBeCompiled['userID']);
                return array(
                    "type" => "compile",
                    "user" => $needToBeCompiled);
                }

                // Assign a run game tasks
                $possibleNumPlayers = array(2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6);
                $numPlayers = $possibleNumPlayers[array_rand($possibleNumPlayers)];

                $users = $this->selectMultiple("SELECT * FROM User WHERE isRunning=1 ORDER BY rand()");
                $seedPlayer = $this->select("select * from User where isRunning=1 and (numGames < 20 or didTimeout < 0.9) order by rand()*-pow(sigma, 2)");

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
                if(count($seedPlayer) < 1) return null;
                $players = $this->selectMultiple("SELECT * FROM User WHERE isRunning=1 and ABS(rank-{$seedPlayer['rank']}) < (5 / pow(rand(), 0.65)) and userID <> {$seedPlayer['userID']} ORDER BY rand() LIMIT ".($numPlayers-1));
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
            $language = isset($_POST['language']) ? $_POST['language'] : "Other";
            $user = $this->select("SELECT * FROM User WHERE userID={$userID}")['email'];

            if($didCompile == 1) { // Did succeed
                $this->sendEmail($user['email'], "Compilation Success", "Your bot was sucessfully compiled on our servers as a program written in {$language}. Within a few minutes, your bot will begin playing games against other contestant's programs. Replays of these games will show up on your homepage: {$WEB_DOMAIN}user.php?userID={$userID}");

                $numActiveUsers = mysqli_query($this->mysqli, "SELECT userID FROM User WHERE isRunning=1")->num_rows;
                
                $this->insert("UPDATE User SET numSubmissions=numSubmissions+1, numGames=0, mu = 25.000, sigma = 8.333, compileStatus = 0, isRunning = 1, language = '".$this->mysqli->real_escape_string($language)."' WHERE userID = ".$this->mysqli->real_escape_string($userID));

                $this->insert("INSERT INTO UserHistory (userID, versionNumber, lastRank, lastNumPlayers, lastNumGames) VALUES ({$user['userID']}, {$user['numSubmissions']}, {$user['rank']}, $numActiveUsers, {$user['numGames']})");
            } else { // Didnt succeed
                $this->sendEmail($user['email'], "Compilation FAILURE", "<h2>The bot that you recently submitted to the Halite competition would not compile on our servers.</h2> <p>Our autocompile script <b>thought that your bot was written in \"{$language}\"</b> If that is incorrect, please change your code's file extensions to <code>cpp</code> and <code>h</code> for C++11, <code>java</code> for Java 7, <code>py</code> for Python3, and <code>rs</code> for Rust 1.10. Make sure to include a <code>Cargo.toml</code> file if you are using Rust. Please make sure that your <b>main file is named MyBot</b>.</p> <b>Here is a description of the compilation error</b>:<br><pre><code><br>{$_POST['output']}</code></pre>");
                $this->insert("UPDATE User SET compileStatus = 0 WHERE userID = ".$this->mysqli->real_escape_string($userID));
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

            // Throw out the game if it is using an old version of a person's bot
            foreach($users as $user) {
                $storedUser = $this->select("SELECT numSubmissions FROM User WHERE userID=".$this->mysqli->real_escape_string($user->userID));
                if(intval($storedUser['numSubmissions']) != intval($user->numSubmissions)) {
                    return null;
                }
            }

            // Store replay file and error logs
            $replayName = null;
            $s3Client = $this->loadAwsSdk()->createS3();
            foreach($_FILES as $fileKey => $file) {
                $pathParts = pathinfo($file['name']);
                $args = [
                    'Key'    => $pathParts['basename'],
                    'Body'   => file_get_contents($file['tmp_name']) 
                ];

                if(strcmp('hlt', $pathParts['extension']) == 0) {
                    $replayName = $pathParts['basename'];
                    $args["Bucket"] = REPLAY_BUCKET;
                    $args["ContentEncoding"] = "gzip";
                } else {
                    $args["Bucket"] = ERROR_LOG_BUCKET;
                }


                $s3Client->putObject($args);
            }


            // Check that we arent stoing too many games in db
            $numAllowed = 50000;
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
            for($a = 0; $a < count($users); $a++) {
                $timeoutInt = $users[$a]->didTimeout ? 1 : 0;
                $errorLogName = $users[$a]->errorLogName == NULL ? "NULL" : "'".$this->mysqli->real_escape_string($users[$a]->errorLogName)."'";
                $this->insert("INSERT INTO GameUser (gameID, userID, errorLogName, rank, playerIndex, didTimeout) VALUES ($gameID, ".$this->mysqli->real_escape_string($users[$a]->userID).", $errorLogName, ".$this->mysqli->real_escape_string($users[$a]->rank).", ".$this->mysqli->real_escape_string($users[$a]->playerTag).", {$timeoutInt})");

                // Cache didTimeout
                // Note: this is written to be agnostic of the number of stats in each row of $gameStats, just in case we want to add more stats
                $gameStats = $this->selectMultiple("SELECT didTimeout FROM GameUser WHERE userID=".$this->mysqli->real_escape_string($users[$a]->userID));
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
                    $this->insert("UPDATE User SET $statName=$averageStatValue WHERE userID = ".$this->mysqli->real_escape_string($users[$a]->userID));
                }
            }
            
            // Increment number of games
            foreach($users as $user) $this->insert("UPDATE User SET numGames=numGames+1 WHERE userID=".$user->$userID); 

            // Send first game email and first timeout email
            foreach($users as $user) {
                $storedUser = $this->select("SELECT numGames,email FROM User WHERE userID=".$user->userID);
                if(intval($storedUser['numGames']) == 3) {
                    $this->sendEmail($email, "First Leaderboard Games", "Your bot has played a couple of games against other contestants' bots. To see them, head over to your homepage <a href='{$WEB_DOMAIN}user.php?userID={$user->userID}'></a>");
                } 

                if($user->didTimeout && mysqli_query("SELECT * from GameUser WHERE didTimeout = 1 and userID={$user->userID}")->num_rows == 1) {
                    $errorLogContents = NULL;
                    foreach($_FILES as $file) {
                        if($file['name'] == $user->errorLogName) {
                            $errorLogContents = file_get_contents($file['tmp_name']);
                            break;
                        }
                    }
                    if($errorLogContents == NULL) continue;

                    $this->sendEmail($email, "First Bot Timeout", "Your bot timed out in a game for the first time. <a href='{$WEB_DOMAIN}game.php?replayName={$replayName}'>Here</a> is a visualization of the game. The game should also be listed in your recent games feed on your <a href='{$WEB_DOMAIN}user.php?userID={$user->userID}'>page</a>. We will <b>not</b> send you emails about subsequent timeouts of your bot. Here is your bot's error log: <br> <pre><code>{$errorLogContents}</code></pre>");
                }
            }

            // Update mu and sigma for the players
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

            // Update overall rank of everyone
            $allUsers = $this->selectMultiple("SELECT * FROM User where isRunning=1");
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
            if(isset($_GET['compile'])) readfile(COMPILE_PATH."{$userID}.zip");
            else readfile(BOTS_PATH."{$userID}.zip");
            exit;
        } else if(isset($_POST['userID']) && count($_FILES) > 0) {
            $userID = $_POST['userID'];
            $key = array_keys($_FILES)[0];
            $name = basename($_FILES[$key]['name']);

            $targetPath = BOTS_PATH."{$userID}.zip";
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
            if(isset($_GET['compile']) && file_exists(COMPILE_PATH."{$userID}.zip")) return array("hash" => md5_file(COMPILE_PATH."{$userID}.zip"));
            else if(!isset($_GET['compile']) && file_exists(BOTS_PATH."{$userID}.zip")) return array("hash" => md5_file(BOTS_PATH."{$userID}.zip"));
            else return "Bot file does not exist";
        }
    }
}
?>
