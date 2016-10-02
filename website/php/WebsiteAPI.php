<?php

require __DIR__ . '/../vendor/autoload.php';

use OAuth\OAuth2\Service\GitHub;
use OAuth\ServiceFactory;
use OAuth\Common\Storage\Session;
use OAuth\Common\Consumer\Credentials;
use OAuth\Common\Http\Uri\UriFactory;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
ini_set('session.gc_maxlifetime', 7*24*3600);

header('Access-Control-Allow-Origin: *');
error_reporting(E_ALL);

date_default_timezone_set('America/New_York');

include dirname(__FILE__).'/API.class.php';
include dirname(__FILE__).'/../lib/swiftmailer/lib/swift_required.php';

define("INI_PATH", dirname(__FILE__)."/../../halite.ini");
define("COMPILE_PATH", dirname(__FILE__)."/../../storage/compile/");
define("BOTS_PATH", dirname(__FILE__)."/../../storage/bots/");
define("ERRORS_PATH", dirname(__FILE__)."/../../storage/errors/");
define("REPLAYS_PATH", dirname(__FILE__)."/../../storage/replays/");
define("ORGANIZATION_WHITELIST_PATH", dirname(__FILE__)."/../../organizationWhitelist.txt");
define("USER_TO_SERVER_RATIO", 20);
define("WORKER_LIMIT", 50);

class WebsiteAPI extends API{
    // The database
    private $mysqli = NULL;

    public function __construct($request) {
        if (session_status() == PHP_SESSION_NONE) {
            session_start();
        }

        $this->config = parse_ini_file(INI_PATH, true);

        $this->initDB();

        $this->sanitizeHTTPParameters();

        parent::__construct($request);
    }
    
    /* Apply MYSQL sanitization to all incoming parameters
     * 
     * TODO: take this out, switch to just sanitizing when necessary
     * Sanitizing everything breaks json parsing of params
     * Also, bad practice
     */
    private function sanitizeHTTPParameters() {
        foreach ($_POST as $key => $value) {
            $_POST[$key] = $this->mysqli->real_escape_string($value);
        }
    }
    
    // Initializes and returns a mysqli object that represents our mysql database
    private function initDB() {
        $this->mysqli = new mysqli($this->config['database']['hostname'],
            $this->config['database']['username'],
            $this->config['database']['password'],
            $this->config['database']['name']);

        if (mysqli_connect_errno()) {
            echo "<br><br>There seems to be a problem with our database. Reload the page or try again later.";
            exit();
        }
    }
    
    // Gets the id of one of our users in the discourse forums system
    private function getForumsID($userID) {
        $url = "http://forums.halite.io/users/by-external/{$userID}.json/?".http_build_query(array('api_key' => $this->config['forums']['apiKey'], 'api_username' => $this->config['forums']['apiUsername']));
        $contents = file_get_contents($url);
        return intval(json_decode($contents, true)['user']['id']);
    }
    
    // Log a users out of forums.halite.io
    private function logOutForums($forumsID) {
        $options = array('http' => array(
            'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
            'method'  => 'POST',
            'content' => http_build_query(array('api_key' => $this->config['forums']['apiKey'], 'api_username' => $this->config['forums']['apiUsername']))
        ));
        file_get_contents("http://forums.halite.io/admin/users/{$forumsID}/log_out", false, stream_context_create($options));
    }
    
    // Select one element from our MySQL db
    private function select($sql) {
        $res = mysqli_query($this->mysqli, $sql);
        return mysqli_fetch_array($res, MYSQLI_ASSOC);
    }
    
    // Select all available elements from our MySQL db
    private function selectMultiple($sql) {
        $res = mysqli_query($this->mysqli, $sql);
        $finalArray = array();

        while($temp = mysqli_fetch_array($res, MYSQLI_ASSOC)) {
            array_push($finalArray, $temp);
        }

        return $finalArray;
    }
    
    // Perform a non-select (insert, update, delete) query
    // TODO: insert is a terrible name for this method
    private function insert($sql) {
        mysqli_query($this->mysqli, $sql);
    }

    private function isLoggedIn() {
        return isset($_SESSION['userID']) && mysqli_query($this->mysqli, "SELECT * FROM User WHERE userID={$_SESSION['userID']}")->num_rows == 1;
    }

    private function getUsers($query) {
        $users = $this->selectMultiple($query);
        foreach($users as &$user) unset($user['email']);
		return $users;
    }

    private function getLoggedInUser() {
        return $this->getUsers("SELECT * FROM User WHERE userID={$_SESSION['userID']}")[0];
    }


    //------------------------------------- API ENDPOINTS ----------------------------------------\\
    // Endpoint associated with a users credentials (everything in the User table; i.e. name, oauthID, etc.)
    // -------------------------------------------------------------------------------------------\\

    /* User Endpoint
     *
     * Encapsulates user information.
     */ 
    protected function user() {
        // Get a user's info with a username        
        if(isset($_GET["username"])) {
            return $this->getUsers("SELECT * FROM User WHERE username = '{$_GET['username']}'")[0];
        } 
        
        // Get a user's info with a userID
        else if (isset($_GET["userID"])) {
            return $this->getUsers("SELECT * FROM User WHERE userID = '{$_GET['userID']}'")[0];
        } 
        
        // Get a set of filtered users
        else if(isset($_GET['fields']) && isset($_GET['values'])) {
            $limit = isset($_GET['limit']) ? $_GET['limit'] : 10;
            $whereClauses = array_map(function($a) {return $_GET['fields'][$a]." = '".$_GET['values'][$a]."'";}, range(0, count($_GET['fields'])-1));
            $orderBy = isset($_GET['orderBy']) ? $_GET['orderBy'] : 'userID';
            $page = isset($_GET['page']) ? $_GET['page'] : 0;

            $results = $this->getUsers("SELECT * FROM User WHERE ".implode(" and ", $whereClauses)." ORDER BY ".$orderBy." LIMIT ".$limit." OFFSET ".($limit*$page));
            $isNextPage = count($this->getUsers("SELECT * FROM User WHERE ".implode(" and ", $whereClauses)." ORDER BY ".$orderBy." LIMIT 1 OFFSET ".($limit*($page+1)))) > 0;

            foreach(array_keys($results) as $key) unset($results[$key]["email"]);

            return array("isNextPage" => $isNextPage, "users" => $results);
        } 

        // Get all of the user's with active submissions
        else if(isset($_GET['active'])) {
            return $this->getUsers("SELECT * FROM User WHERE isRunning=1");
        } 
        
        // Github calls this once a user has granted us access to their profile info
        if(isset($_GET["githubCallback"]) && isset($_GET["code"])) {
            $code = $_GET["code"];

            $serviceFactory = new ServiceFactory();
            $credentials = new Credentials($this->config['oauth']['githubClientID'], $this->config['oauth']['githubClientSecret'], NULL);
            $gitHub = $serviceFactory->createService('GitHub', $credentials, new Session(), array('user'));
            $gitHub->requestAccessToken($code);
            $githubUser = json_decode($gitHub->request('user'), true);
            var_dump($githubUser);

            if(mysqli_query($this->mysqli, "SELECT userID FROM User WHERE oauthProvider=1 and oauthID={$githubUser['id']}")->num_rows > 0) { // Already signed up
                
                $_SESSION['userID'] = $this->select("SELECT userID FROM User WHERE oauthProvider=1 and oauthID={$githubUser['id']}")['userID'];
            } else { // New User
                $organization = "Other";
                if($githubUser['email'] != NULL) {
                    $emailDomain = explode('@', $githubUser['email'])[1];
                    $rows = explode("\n", file_get_contents(ORGANIZATION_WHITELIST_PATH));
                    foreach($rows as $row) {
                        $components = explode(" - ", $row);
                        if(strcmp($components[1], $emailDomain) == 0) {
                            $organization = $components[0];
                            break;
                        }
                    }
                }

                $numActiveUsers = mysqli_query($this->mysqli, "SELECT userID FROM User WHERE isRunning=1")->num_rows + 1; // Add once since this user hasnt been inserted into the db
                $this->insert("INSERT INTO User (username, email, organization, oauthID, oauthProvider, rank) VALUES ('{$githubUser['login']}', '{$githubUser['email']}', '{$organization}', {$githubUser['id']}, 1, {$numActiveUsers})");
                $_SESSION['userID'] = $this->mysqli->insert_id;

                // AWS auto scaling
                /*
                $numWorkers = mysqli_query($this->mysqli, "SELECT workerID FROM Worker")->num_rows;
                if($numWorkers > 0 && $numWorkers < WORKER_LIMIT && $numActiveUsers / (float)$numWorkers < USER_TO_SERVER_RATIO) {
                    shell_exec("python3 openNewWorker.py &");
                }*/
            }

            if(isset($_GET['redirectURL'])) header("Location: {$_GET['redirectURL']}");
            else header("Location: http://halite.io/website");
            die();
        } 
    }

    /* User History Endpoint
     *
     * We store the a history of user's bot submissions.
     * This mitigates the 'stealth' submission problem, 
     * where users submit a bot, wait for their rank to stabilize, and take it down for the sake of secrecy.
     */
    protected function history() {
        if(isset($_GET["userID"])) {
            return $this->selectMultiple("SELECT * FROM UserHistory WHERE userID={$_GET["userID"]} ORDER BY versionNumber DESC");
        }
    }

    /* Game Endpoint
     *
     * Games are continuously run on our servers and exposed on our website
     */
    protected function game() {
        if(isset($_GET['userID'])) {
            $limit = isset($_GET['limit']) ? $_GET['limit'] : 5;
            $startingID = isset($_GET['startingID']) ? $_GET['startingID'] : PHP_INT_MAX;
            $userID = $_GET['userID'];

            $gameIDArrays = $this->selectMultiple("SELECT gameID FROM GameUser WHERE userID = $userID and gameID < $startingID ORDER BY gameID DESC LIMIT $limit");
            $gameArrays = array();

            // Get each game's info
            foreach ($gameIDArrays as $gameIDArray) {
                $gameID = $gameIDArray['gameID'];
                $gameArray = $this->select("SELECT * FROM Game WHERE gameID = $gameID");

                // Get information about users
                $gameArray['users'] = $this->selectMultiple("SELECT userID, errorLogName, rank FROM GameUser WHERE gameID = $gameID");
                foreach($gameArray['users'] as &$gameUserRow) {
                    // Get rid of gameID
                    unset($gameUserRow['gameID']);

                    // Add in user info
                    $userInfo = $this->select("SELECT username FROM User WHERE userID = {$gameUserRow['userID']}");
                    foreach($userInfo as $key => $value) $gameUserRow[$key] = $value;
                }
                array_push($gameArrays, $gameArray);
            }
            return $gameArrays;
        } else if(isset($_GET['random'])) {
            return $this->select("SELECT replayName FROM (SELECT * FROM Game ORDER BY gameID DESC LIMIT 50) sub ORDER BY rand() LIMIT 1");

        }
    }
    
    /* Bot File Endpoint
     *
     * Handles the user's submission of a new bot
     */
    protected function botFile() {
        // Mark a new botfile for compilation if valid. Return error otherwise 
        if($this->isLoggedIn() && isset($_FILES['botFile']['name'])) {
            $user = $this->getLoggedInUser();
            
            if($user['compileStatus'] != 0) {
                return "Compiling";
            }
            
            if ($_FILES["botFile"]["size"] > 20000000) {
                return "Sorry, your file is too large.";
            }

            $targetPath = COMPILE_PATH."{$user['userID']}.zip";
            if(file_exists($targetPath))  {
                unlink($targetPath);    
            }

            if(!move_uploaded_file($_FILES['botFile']['tmp_name'], $targetPath)) {
                if(!copy($_FILES['botFile']['tmp_name'], $targetPath)) {
                    return null;
                }
            }

            $oldGameUsers = $this->selectMultiple("SELECT gameID FROM GameUser WHERE userID={$user['userID']}");
            foreach($oldGameUsers as $oldGameUser) {
                $this->insert("DELETE FROM Game WHERE gameID={$oldGameUser['gameID']}");
                $this->insert("DELETE FROM GameUser WHERE gameID={$oldGameUser['gameID']}");
            }
            
            $this->insert("UPDATE User SET compileStatus = 1 WHERE userID = {$user['userID']}");

            return "Success";
        }
    }
    
    /* Forums Endpoint
     *
     * Handle the Discourse forums (forums.halite.io) single sign on authentication.
     */
    protected function forums() {
        // Follows the Discource sso detailed here: https://meta.discourse.org/t/official-single-sign-on-for-discourse/13045
        if(isset($_GET['sso']) && isset($_GET['sig'])) {
            if(!$this->isLoggedIn()) {
                $forumsCallbackURL = urlencode("http://halite.io/website/php/forums?".http_build_query(array("sig" => $_GET['sig'], "sso" => $_GET['sso'])));
                $githubCallbackURL = urlencode("http://halite.io/website/php/user?githubCallback=1&redirectURL={$forumsCallbackURL}");
                header("Location: https://github.com/login/oauth/authorize?scope=user:email&client_id=2b713362b2f331e1dde3&redirect_uri={$githubCallbackURL}");
                die();
            }

            $user = $this->getLoggedInUser();

            $initialBase64Payload = stripcslashes($_GET['sso']);
            $signature = $_GET['sig'];

            $correctSignature = hash_hmac("sha256", $initialBase64Payload, $this->config['sso']['secret']);

            if($correctSignature != $signature) {
                return null;
            }

            parse_str(base64_decode($initialBase64Payload), $initialPayload);
            $nonce = $initialPayload["nonce"];

            $finalBase64Payload = base64_encode(http_build_query(array(
                "nonce" => $nonce,
                "name" => $user['username'],
                "email" => $user['email'],
                "external_id" => $user['userID']
            )));
            $finalSignature = hash_hmac("sha256", $finalBase64Payload, $this->config['sso']['secret']);

            $finalQueryString = http_build_query(array(
                "sso" => $finalBase64Payload,
                "sig" => $finalSignature
            ));
            $finalURL = $this->config['sso']['url']."?".$finalQueryString;

            header("Location: ".$this->config['sso']['url']."?".$finalQueryString);
            die();
        }
    }
    
    /* Worker Endpoint
     *
     * Bots are compiled and run in games by a decentralized network of 'worker' servers.
     * A few stats about each worker are recorded by our 'manager' server.
     * These stats are exposed on our status page.
     */
    protected function worker() {
        $workers = $this->selectMultiple("SELECT * FROM Worker ORDER BY workerID");
        return $workers;
    }
    
    /* Stats endpoint
     *
     * Provides a number of statistics about the competition,
     * which would be expensive or impossible to determine using our generic endpoints.
     */ 
    protected function stats() {
        if(isset($_GET['throughput'])) {
            return mysqli_query($this->mysqli, "SELECT * FROM Game WHERE TIMESTAMPDIFF(DAY, timestamp, NOW()) < 1")->num_rows;
        }

        // Get the number of active users
        else if(isset($_GET['numSubmissions'])) {
            return $this->select("SELECT SUM(numSubmissions) FROM User")["SUM(numSubmissions)"];
        }

        // Get the number of active users
        else if(isset($_GET['numActive'])) {
            return mysqli_query($this->mysqli, "SELECT userID FROM User WHERE isRunning=1")->num_rows;
        } 
    }

    /* Announcement Endpoint
     *
     * Annoucements are used as 'news blasts' without requiring email.
     * An alert with the annoucement's body and header is showed to users.
     * Once it has been closed, it is never shown to that user again
     */
    protected function announcement() {
        // Get the newest annoucement available to a user
        if(isset($_GET['userID'])) {
            return $this->select("SELECT a.* FROM Announcement a WHERE NOT EXISTS(SELECT NULL FROM DoneWithAnnouncement d WHERE d.userID = {$_GET['userID']} and d.announcementID = a.announcementID) ORDER BY announcementID LIMIT 1;");
        } 
        
        // Mark an annoucement as closed    
        else if(isset($_POST['announcementID'])) {
            $announcementID = $_POST['announcementID'];
            $user = $this->getLoggedInUser();

            if(count($this->select("SELECT * FROM User WHERE user={$user['userID']} LIMIT 1")) > 0) {
                $this->insert("INSERT INTO DoneWithAnnouncement (userID, announcementID) VALUES ({$user['userID']}, $announcementID)");
                return "Success";
            }
            return "Fail";
        }
    }
    
    /* Error Log Endpoint
     *
     * When a users times-out or errors during a game of Halite on our server,
     * we store their stdout and stderr output and make it available to them.
     * Users may only see **their** error logs.
     */
    protected function errorLog() {

        // Return the requested error log only if it belongs to the signed in user.
        if(isset($_GET['errorLogName']) && count($this->select("SELECT * FROM GameUser WHERE errorLogName='{$_GET['errorLogName']}' and userID={$_SESSION['userID']}"))) {
            $targetPath = ERRORS_PATH."{$_GET['errorLogName']}"; 
            if(file_exists($targetPath) == false) {
                return null;
            }

            header($_SERVER["SERVER_PROTOCOL"] . " 200 OK");
            header("Cache-Control: public"); // needed for internet explorer
            header("Content-Type: application/txt");
            header("Content-Transfer-Encoding: Binary");
            header("Content-Length:".filesize($targetPath));
            header("Content-Disposition: attachment; filename=error.log");
            readfile($targetPath);
            die();
        }
    }
    
    /* Session Endpoint
     *
     * Encapsulates the logged in user's info
     */
    protected function session() {

        // Get the logged in user's info
        if($this->method == 'GET') {
            if(count($_SESSION) > 0) return $_SESSION;
            else return NULL;
        } 
        
        // Log out a user
        else if($this->method == 'DELETE') {
            if(isset($_SESSION['userID'])) {
                $this->logOutForums($this->getForumsID($_SESSION['userID']));
            }
            session_destroy();
            return "Success";
        }
    }
 }

 ?>
