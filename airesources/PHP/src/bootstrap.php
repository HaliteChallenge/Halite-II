<?php
ini_set("log_errors", 1);
ini_set("error_log",  __DIR__.'../php-error.log');
spl_autoload_register(
    function ($class) {
        $path = __DIR__.'/'.$class.'.php';
        if (is_readable($path)) {
            include $path;
        }
    }
);
