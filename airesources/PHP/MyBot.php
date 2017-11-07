<?php

require_once __DIR__.'/src/bootstrap.php';

$logger = new Logger();
$connection = new Connection($logger);
$game = new Game('Settler_'.uniqid(null, true), $logger, $connection);
$turn = 0;
while (true) {
    $turn++;
    $logger->log('--------- NEW TURN ------------');
    $logger->log('');
    $logger->log('');
    $map = $game->updateMap();
    if($turn % 2=== 0){
        foreach ($map->getMe()->getShips() as $ship) {
            $connection->move($ship->thrust(0,0));
        }
        $connection->flush();
        continue;
    }
    foreach ($map->getMe()->getShips() as $ship) {
        $logger->log('New Turn with ship '.json_encode($ship));
        if ($ship->isDocked() || $ship->getDockingProgress() > 0) {
            continue;
        }
        $planet = $map->getNextDockablePlanet($ship);
        if ($ship->canDock($planet)) {
            $logger->log('Ship can dock planet -> send dock move');
            $connection->move($ship->dock($planet->getId()));
        } else {
            $target = $ship->getClosestCoordinateTo($planet);
            $navigate = $ship->navigate($map, $target, 7, true, 10, 0.5, $logger);
            $willCollide = false;
            foreach ($map->getMe()->getShips() as $otherShip) {
                if ($ship->getId() === $otherShip->getId() || $otherShip->getCoordinateNextTurn() === null || $ship->getCoordinateNextTurn()  === null ) {
                    continue;
                }
                $willCollide |= $ship->getCoordinateNextTurn()->hasCollision($ship->getRadius(),$otherShip->getCoordinateNextTurn(), $otherShip->getRadius());
            }
            if($willCollide){
                $logger->log('will collide stay');
            }else{
                $logger->log('Cannot dock -> navigate to: '.$navigate);
                $connection->move($navigate);
            }

        }
    }
    $connection->flush();
}
