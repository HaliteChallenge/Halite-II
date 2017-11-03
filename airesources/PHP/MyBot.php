<?php

require_once __DIR__.'/src/bootstrap.php';

$logger = new Logger();
$connection = new Connection($logger);
$game = new Game('Settler_'.uniqid(null, true), $logger, $connection);

while (true) {
    $logger->log('--------- NEW TURN ------------');
    $logger->log('');
    $logger->log('');
    $map = $game->updateMap();
    foreach ($map->getMe()->getShips() as $ship) {
        $logger->log('New Turn with ship '.json_encode($ship));
        if ($ship->isDocked() || $ship->getDockingProgress() > 0) {
            continue;
        }
        $planets = $map->getNearbyEntitiesByDistance($ship, true, false);
        $planet = $planets->current();
        /** @var $planet Planet */
        if ($ship->canDock($planet)) {
            $logger->log('Ship can dock planet -> send dock move');
            $connection->move($ship->dock($planet->getId()));
        } else {
            $target = $ship->getClosestCoordinateTo($planet);
            $navigate = $ship->navigate($map, $target, 5, false, 10, 0.1, $logger);
            $logger->log('Cannot dock -> navigate to: '.$navigate);
            $connection->move($navigate);
        }
    }
    $connection->flush();
}
