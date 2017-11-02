<?php

require_once __DIR__.'/src/bootstrap.php';

$logger = new Logger();
$connection = new Connection($logger);
$game = new Game('Settler_'.uniqid(null, true), $logger, $connection);

while (true) {
    $map = $game->updateMap();
    foreach ($map->getMe()->getShips() as $ship) {
        $planets = $map->getNearbyEntitiesByDistance($ship, true, false);
        $planet = $planets->current();
        if ($ship->canDock($planet)) {
            $connection->sendMove($ship->dock($planet->getId()));
        } else {
            $navigate = $ship->navigate($map, $planet->getCoordinate(), 3, false, 10, 0.1, $logger);
            $logger->log('Navigate: '.$navigate);
            $connection->sendMove($navigate ?: $ship->thrust(2, 90));
        }
    }
}
