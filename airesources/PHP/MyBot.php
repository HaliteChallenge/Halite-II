<?php

require_once __DIR__.'/src/bootstrap.php';

$logger = new Logger();
$connection = new Connection($logger);
$game = new Game('Settler_'.uniqid(null, true), $logger, $connection);

while (true) {
    $map = $game->updateMap();
    foreach ($map->getMe()->getShips() as $ship) {
        $logger->log($ship->getDockingProgress() . 'ddddd');
        if ($ship->isDocked() || $ship->getDockingProgress() > 0) {
            continue;
        }

        $planets = $map->getNearbyEntitiesByDistance($ship, true, false);
        $planet = $planets->current();
        /** @var $planet Planet */
        if ($ship->canDock($planet)) {
            $connection->sendMove($ship->dock($planet->getId()));
        } else {
            $target = $ship->getClosestCoordinateTo($planet);
            $navigate = $ship->navigate($map, $target, 7, false, 10, 0.1, $logger);
            $logger->log('Navigate: '.$navigate);
            $connection->sendMove($navigate );
        }
    }
}
