<?php

require_once __DIR__.'/src/bootstrap.php';

$logger = new Logger();
$connection = new Connection($logger);
$game = new Game('Settler_'.uniqid(null, true), $logger, $connection);

while (true) {
    $map = $game->updateMap();
}
