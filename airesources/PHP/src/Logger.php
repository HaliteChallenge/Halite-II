<?php

class Logger
{
    /**
     * @var string
     */
    private static $filename;

    /**
     * @var bool
     */
    private static $enabled;

    public static function init(int $botId, string $botName)
    {
        self::$enabled = getenv('HALITE_PHP_ENV') === 'dev';
        if (!self::$enabled) {
            return;
        }

        $dir = __DIR__.'/../logs';
        if (!is_dir($dir)) {
            mkdir($dir);
        }

        self::$filename = $dir.'/'.$botId.'-'.$botName.'.log';
        if (file_exists(self::$filename)) {
            unlink(self::$filename);
        }
    }

    public static function log(string $message)
    {
        if (self::$enabled) {
            file_put_contents(self::$filename, $message."\n", FILE_APPEND);
        }
    }
}
