<?php

use Aws\Sdk;
require __DIR__ . '/../../vendor/autoload.php';
require_once '../API.class.php';

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

    private function checkConfig($section, $key) {
        if(!isset($this->config[$section]) || !isset($this->config[$section][$key])) {
            return false;
        }
        return $this->config[$section][$key];
    }


    /////////////////////////API ENDPOINTS\\\\\\\\\\\\\\\\\\\\\\\\\\\\

    // Delegate a task to a workers
    protected function task() {
        if($this->method == 'GET') {
            // Check for compile tasks
            $needToBeCompiled = $this->select("SELECT * FROM User WHERE compileStatus=1 ORDER BY userID ASC LIMIT 1");
            if(count($needToBeCompiled) > 0) {
                $this->insert("UPDATE User SET compileStatus = 2 WHERE userID = ".$needToBeCompiled['userID']);
                return array(
                    "type" => "compile",
                    "user" => $needToBeCompiled
                );
            }

            // don't give out any task if games have been stopped.
            if($this->checkConfig("compState", "noGameTasks")) {
                return array("type" => "notask");
            }

            // Assign a run game tasks
            $possibleNumPlayers = array(2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6);
            $numPlayers = $possibleNumPlayers[array_rand($possibleNumPlayers)];

            $seedPlayer = null;
            if(!$this->checkConfig("compState", "finalsPairing")) {
                $randValue = mt_rand() / mt_getrandmax();
                if($randValue > 0.5) {
                    $seedPlayer = $this->select("SELECT * FROM User WHERE isRunning = 1 and numGames < 400 order by rand()*-pow(sigma, 2) LIMIT 1");
                }
                if ($randValue > 0.25 && $randValue <= 0.5) {
                    $seedPlayer = $this->select("SELECT * FROM (SELECT u.* FROM (SELECT MAX(g.timestamp) as maxTime, gu.userID as userID FROM GameUser gu INNER JOIN Game g ON g.gameID=gu.gameID GROUP BY gu.userID) temptable INNER JOIN User u on u.userID = temptable.userID where numGames < 400 and isRunning = 1 order by maxTime ASC limit 15) orderedTable order by rand() limit 1;");
                }
                if($randValue <= 0.25 || count($seedPlayer) < 1) {
                    $seedPlayer = $this->select("SELECT u.* FROM (SELECT MAX(g.timestamp) as maxTime, gu.userID as userID FROM GameUser gu INNER JOIN Game g ON g.gameID=gu.gameID GROUP BY gu.userID) temptable INNER JOIN User u on u.userID = temptable.userID where isRunning = 1 order by maxTime ASC limit 1");
                }
            } else {
                $seedPlayer = $this->select("SELECT * FROM (SELECT * FROM User WHERE rank < 398 and isRunning = 1 ORDER BY numGames ASC limit 15) orderedTable ORDER BY rand() LIMIT 1");
            }

            $muRankLimit = intval(5.0 / pow((float)mt_rand(1, mt_getrandmax())/(float)mt_getrandmax(), 0.65));
            $players = $this->selectMultiple("SELECT * FROM (SELECT * FROM User WHERE isRunning=1 and userID <> {$seedPlayer['userID']} ORDER BY ABS(mu-{$seedPlayer['mu']}) LIMIT $muRankLimit) muRankTable ORDER BY rand() LIMIT ".($numPlayers-1));
            array_unshift($players, $seedPlayer);

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
            $user = $this->select("SELECT * FROM User WHERE userID={$userID}");

            if($didCompile == 1) { // Did succeed
                $this->sendNotification($user, "Compilation Success", "<p>Your bot was sucessfully compiled on our servers as a program written in {$language}. Within a few minutes, your bot will begin playing games against other contestant's programs. Replays of these games will show up on your <a href='".WEB_DOMAIN."user.php'>homepage</a>.</p>", 1);

                $this->insert("UPDATE User SET numSubmissions=numSubmissions+1, numGames=0, mu = 25.000, sigma = 8.333, compileStatus = 0, isRunning = 1, language = '".$this->mysqli->real_escape_string($language)."' WHERE userID = ".$this->mysqli->real_escape_string($userID));

                if(intval($user['numSubmissions']) != 0) {
                    $numActiveUsers = mysqli_query($this->mysqli, "SELECT userID FROM User WHERE isRunning=1")->num_rows;
                    $this->insert("INSERT INTO UserHistory (userID, versionNumber, lastRank, lastNumPlayers, lastNumGames) VALUES ({$user['userID']}, {$user['numSubmissions']}, {$user['rank']}, $numActiveUsers, {$user['numGames']})");
                }
            } else { // Didnt succeed
                $this->sendNotification($user, "Compilation Failure", "<p>The bot that you recently submitted to the Halite competition would not compile on our servers.</p><p>Our autocompile script <b>thought that your bot was written in \"{$language}\"</b>.</p><b>Here is a description of the compilation error</b>:<br><pre><code><br>{$_POST['errors']}</code></pre>", -1);
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
            $workerID = $this->select("SELECT workerID FROM Worker WHERE apiKey=".$this->mysqli->real_escape_string($this->apiKey))["workerID"];

            $mapWidth = $_POST['mapWidth'];
            $mapHeight = $_POST['mapHeight'];
            $users = json_decode($_POST['users'], true);

            // Throw out the game if it is using an old version of a person's bot
            foreach($users as $key => $user) {
                // Will need email credentials for email sending, numSubmissions for version checking, and mu + sigma so we can update trueskill
                $storedUser = $this->select("SELECT userID, onEmailList, email, numSubmissions, mu, sigma FROM User WHERE userID=".$this->mysqli->real_escape_string($user['userID']));
                if(intval($storedUser['numSubmissions']) != intval($user['numSubmissions'])) {
                    return null;
                }
                $users[$key] = array_merge($user, $storedUser); // Second param overwrites first param
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
            $numAllowed = 600000;
            $numRows = $this->numRows("SELECT COUNT(*) FROM Game");
            $numToDelete = $numRows - $numAllowed;
            if($numToDelete > 0) {
                $gamesToDelete = $this->selectMultiple("SELECT gameID FROM Game ORDER BY gameID LIMIT $numToDelete");
                foreach($gamesToDelete as $game) {
                    $this->insert("DELETE FROM GameUser WHERE gameID={$game['gameID']}");
                    $this->insert("DELETE FROM Game WHERE gameID={$game['gameID']}");
                }
            }

            // Store game information in db
            $this->insert("INSERT INTO Game (replayName, mapWidth, mapHeight, timestamp, workerID) VALUES ('".$this->mysqli->real_escape_string($replayName)."', ".$this->mysqli->real_escape_string($mapWidth).", ".$this->mysqli->real_escape_string($mapHeight).", NOW(), $workerID)");
            $gameID = $this->mysqli->insert_id;

            // Update each participant's stats
            for($a = 0; $a < count($users); $a++) {
                $timeoutInt = $users[$a]['didTimeout'] ? 1 : 0;
                $errorLogName = $users[$a]['errorLogName'] == NULL ? "NULL" : "'".$this->mysqli->real_escape_string($users[$a]['errorLogName'])."'";
                $this->insert("INSERT INTO GameUser (gameID, userID, errorLogName, rank, playerIndex, didTimeout, versionNumber) VALUES ($gameID, ".$this->mysqli->real_escape_string($users[$a]['userID']).", $errorLogName, ".$this->mysqli->real_escape_string($users[$a]['rank']).", ".$this->mysqli->real_escape_string($users[$a]['playerTag']).", {$timeoutInt}, {$users[$a]['numSubmissions']})");

                // Increment number of games
                $this->insert("UPDATE User SET numGames=numGames+1 WHERE userID=".$users[$a]['userID']); 
            }
            

            // Send first game email and first timeout email
            foreach($users as $user) {
                if($user['didTimeout'] && mysqli_query($this->mysqli, "SELECT * from GameUser WHERE didTimeout = 1 and versionNumber = {$user['numSubmissions']} and userID={$user['userID']}")->num_rows == 1) {
                    $errorLogContents = NULL;
                    foreach($_FILES as $file) {
                        if($file['name'] == $user['errorLogName']) {
                            $errorLogContents = file_get_contents($file['tmp_name']);
                            break;
                        }
                    }
                    if($errorLogContents == NULL) continue;

                    $message = "<p>Your bot timed out in a game for the first time. This happens when your bot doesn't respond in 15 seconds of a game's start or 1 second of a turn's start. A timeout may be the result of a runtime error in your bot. When your bot times out, its pieces become part of the map and it is ejected from the game.</p> <p><a href='".WEB_DOMAIN."game.php?replay={$replayName}'>Here</a> is a visualization of the game in which your bot timed out.</p> <p><a href='".WEB_DOMAIN."api/web/errorLog?errorLogName={$user['errorLogName']}'>Here</a> is your bot's error log. An error log contains your bot's output (from stdout and stderr) and the time it took per turn.</p>";
                    $this->sendNotification($user, "First Bot Timeout/Error", $message, -1);
                }
            }

            // Update mu and sigma for the players
            usort($users, function($a, $b) {
                return $a['rank'] > $b['rank'];
            });
            $rankings = array();
            foreach($users as $user) {
                array_push($rankings, $user['mu']);
                array_push($rankings, $user['sigma']);
            }
            exec("python3 updateTrueskill.py ".implode(' ', $rankings), $lines);
            var_dump($lines);
            for($a = 0; $a < count($users); $a++) {
                $components = explode(' ', $lines[$a]);
                $this->insert("UPDATE User SET mu=".$this->mysqli->real_escape_string($components[0]).", sigma=".$this->mysqli->real_escape_string($components[1])." WHERE userID=".$this->mysqli->real_escape_string($users[$a]['userID'])." AND numSubmissions=".$this->mysqli->real_escape_string($users[$a]['numSubmissions']));
            }

            // Update overall rank of everyone
            $allUsers = $this->selectMultiple("SELECT * FROM User where isRunning=1");
            usort($allUsers, function($a, $b) {
                return $a['mu']-3*$a['sigma'] < $b['mu']-3*$b['sigma'];
            });
            $query = "UPDATE User set rank = CASE";
            for($userIndex = 0; $userIndex < count($allUsers); $userIndex++) {
                $rank = $userIndex+1;
                $query .= " WHEN userID = {$allUsers[$userIndex]['userID']} THEN {$rank}";
            }
            $query .= " ELSE rank END;";
            $this->insert($query);
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

            $bucket = null;
            if(isset($_GET['compile'])) $bucket = COMPILE_BUCKET; 
            else $bucket = BOT_BUCKET; 

            echo $this->loadAwsSdk()->createS3()->getObject([
                'Bucket' => $bucket,
                'Key'    => "$userID" 
            ])['Body'];

            exit;
        } else if(isset($_POST['userID']) && count($_FILES) > 0) {
            $userID = $_POST['userID'];
            $key = array_keys($_FILES)[0];
            $name = basename($_FILES[$key]['name']);

            $this->loadAwsSdk()->createS3()->putObject([
                'Key'    => "{$userID}",
                'Body'   => file_get_contents($_FILES[$key]['tmp_name']),
                'Bucket' => BOT_BUCKET
            ]);
        } else {
            return NULL;
        }

        return "Success";
    }

    // Allow workers to get the hash of a bot file so that they know that they downloaded the file correctly
    protected function botHash() {
        if(isset($_GET['userID'])) {
            $userID = $_GET['userID'];
            $s3Client = $this->loadAwsSdk()->createS3();            
            if(isset($_GET['compile']) && $s3Client->doesObjectExist(COMPILE_BUCKET, "$userID")) {
                return array("hash" => md5($s3Client->getObject(['Bucket' => COMPILE_BUCKET, 'Key'    => "$userID"])['Body']));
            } else if(!isset($_GET['compile']) && $s3Client->doesObjectExist(BOT_BUCKET, "$userID")) {
                return array("hash" => md5($s3Client->getObject(['Bucket' => BOT_BUCKET, 'Key'    => "$userID"])['Body']));
            } else {
                return "Bot file does not exist";
            }
        }
    }
}
?>
