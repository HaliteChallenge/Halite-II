<?php

use Aws\Sdk;

require __DIR__ . '/../vendor/autoload.php';

define("INI_PATH", dirname(__FILE__)."/../../halite.ini");
define("COMPILE_BUCKET", "halitecompilebucket");
define("BOT_BUCKET", "halitebotbucket");
define("REPLAY_BUCKET", "halitereplaybucket");
define("ERROR_LOG_BUCKET", "haliteerrorlogbucket");
define("WEB_DOMAIN", "https://halite.io/");

abstract class API{
    /**
     * Property: method
     * The HTTP method this request was made in, either GET, POST, PUT or DELETE
     */
    protected $method = '';
    /**
     * Property: endpoint
     * The Model requested in the URI. eg: /files
     */
    protected $endpoint = '';
    /**
     * Property: verb
     * An optional additional descriptor about the endpoint, used for things that can
     * not be handled by the basic methods. eg: /files/process
     */
    protected $verb = '';
    /**
     * Property: args
     * Any additional URI components after the endpoint and verb have been removed, in our
     * case, an integer ID for the resource. eg: /<endpoint>/<verb>/<arg0>/<arg1>
     * or /<endpoint>/<arg0>
     */
    protected $args = Array();
    /**
     * Property: file
     * Stores the input of the PUT request
     */
    protected $file = Null;

    protected function numRows($sql) {
        return mysqli_fetch_assoc(mysqli_query($this->mysqli, $sql))["COUNT(*)"];
    }

    protected function select($sql) {
        try {
            $res = mysqli_query($this->mysqli, $sql);
            $array = mysqli_fetch_array($res, MYSQLI_ASSOC);
            return $array;
        } catch(Exception $e) {
            return array();
        }
    }

    protected function selectMultiple($sql) {
        $res = mysqli_query($this->mysqli, $sql);
        $finalArray = array();

        while($temp = mysqli_fetch_array($res, MYSQLI_ASSOC)) {
            array_push($finalArray, $temp);
        }

        return $finalArray;
    }

    protected function insert($sql) {
        mysqli_query($this->mysqli, $sql);
    }

    protected function loadConfig() {
        if(!property_exists($this, "config")) $this->config = parse_ini_file(INI_PATH, true);
    }

    protected function loadAwsSdk() {
        $this->loadConfig();
        putenv("AWS_ACCESS_KEY_ID=".$this->config["aws"]["accesskey"]);
        putenv("AWS_SECRET_ACCESS_KEY=".$this->config["aws"]["secretaccesskey"]);

        $sdk = new Aws\Sdk([
            'region'   => 'us-east-1',
            'version'  => 'latest'
        ]);
        return $sdk;
    }

    protected function sendNotification($recipientUser, $subject, $message, $mood, $doSendNotif=true, $alwaysSendEmail=false) {
        $notificationMessage = $this->mysqli->real_escape_string($message);

        if($doSendNotif) {
            $this->insert("INSERT INTO UserNotification (userID, title, body, mood) VALUES ({$recipientUser['userID']}, '{$subject}', '{$notificationMessage}', {$mood})");
        }

        if($recipientUser['onEmailList'] == 1 || $alwaysSendEmail) {
            try {
                $emailMessage = $message."<hr><p style='color: gray; font-size: 14px;'>To unsubscribe to these emails, click <a href='".WEB_DOMAIN."api/web/emailList?unsubscribe=1'>here</a>. To resubscribe, click <a href='".WEB_DOMAIN."api/web/emailList?subscribe=1'>here</a>.</p>";

                $transporter = Swift_SmtpTransport::newInstance('smtp.sendgrid.net', 465, 'ssl')
                    ->setUsername($this->config['email']['username'])
                    ->setPassword($this->config['email']['password']);
                $mailer = Swift_Mailer::newInstance($transporter);
                $emailMessage = Swift_Message::newInstance($subject)
                    ->setFrom(array($this->config['email']['email'] => 'Halite'))
                    ->setTo(array($recipientUser['email']))
                    ->setBody($emailMessage)
                    ->setContentType("text/html");

                $mailer->send($emailMessage);
            } catch (Exception $e) {
            } finally {
            }
        }
    }

    protected function initDB() {
        $this->loadConfig();

        $this->mysqli = NULL;
        $this->mysqli = new mysqli($this->config['database']['hostname'],
            $this->config['database']['username'],
            $this->config['database']['password'],
            $this->config['database']['name']);

        if (mysqli_connect_errno()) {
            echo "<br><br>There seems to be a problem with our database. Reload the page or try again later.";
            exit();
        }
    }

    /**
     * Constructor: __construct
     * Allow for CORS, assemble and pre-process the data
     */
    public function __construct($request) {
        header("Access-Control-Allow-Orgin: *");
        header("Access-Control-Allow-Methods: *");
        header("Content-Type: application/json");

        $this->loadConfig();

        $this->args = explode('/', rtrim($request, '/'));
        $this->endpoint = array_shift($this->args);
        if (array_key_exists(0, $this->args) && !is_numeric($this->args[0])) {
            $this->verb = array_shift($this->args);
        }

        $this->method = $_SERVER['REQUEST_METHOD'];
        if ($this->method == 'POST' && array_key_exists('HTTP_X_HTTP_METHOD', $_SERVER)) {
            if ($_SERVER['HTTP_X_HTTP_METHOD'] == 'DELETE') {
                $this->method = 'DELETE';
            } else if ($_SERVER['HTTP_X_HTTP_METHOD'] == 'PUT') {
                $this->method = 'PUT';
            } else {
                throw new Exception("Unexpected Header");
            }
        }

        switch($this->method) {
        case 'DELETE':
        case 'POST':
            $this->request = $this->_cleanInputs($_POST);
            break;
        case 'GET':
            $this->request = $this->_cleanInputs($_GET);
            break;
        case 'PUT':
            $this->request = $this->_cleanInputs($_GET);
            $this->file = file_get_contents("php://input");
            break;
        default:
            $this->_response('Invalid Method', 405);
            break;
        }
    }

    public function processAPI() {
        if (method_exists($this, $this->endpoint)) {
            return $this->_response($this->{$this->endpoint}($this->args));
        }
        return $this->_response("No Endpoint: $this->endpoint", 404);
    }

    private function _response($data, $status = 200) {
        header("HTTP/1.1 " . $status . " " . $this->_requestStatus($status));
        return json_encode($data);
    }

    private function _cleanInputs($data) {
        $clean_input = Array();
        if (is_array($data)) {
            foreach ($data as $k => $v) {
                $clean_input[$k] = $this->_cleanInputs($v);
            }
        } else {
            $clean_input = trim(strip_tags($data));
        }
        return $clean_input;
    }

    private function _requestStatus($code) {
        $status = array(
            200 => 'OK',
            404 => 'Not Found',
            405 => 'Method Not Allowed',
            500 => 'Internal Server Error',
        );
        return ($status[$code])?$status[$code]:$status[500];
    }
}

?>
