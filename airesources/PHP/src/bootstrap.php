<?php
spl_autoload_register(
    function ($class) {
        $path = __DIR__.'/'.$class.'.php';
        if (is_readable($path)) {
            include $path;
        }
    }
);
