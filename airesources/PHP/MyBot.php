<?php

require_once __DIR__.'/src/bootstrap.php';

$logger = new Logger(getenv('HALITE_PHP_ENV') === 'dev');
$connection = new Connection($logger);
$game = new Game(uniqid('PHPSettler_'), $logger, $connection);
$turn = 0;
while (true) {
    $turn++;
    $logger->log('--------- NEW TURN ------------');
    $logger->log('');
    $logger->log('');
    $map = $game->updateMap();
    foreach ($map->getMe()->getShips() as $ship) {
        $logger->log('New Turn with ship '.json_encode($ship));
        if ($ship->isDocked() || $ship->getDockingProgress() > 0) {
            continue;
        }
        $planet = $map->getNextDockablePlanet($ship);

        if (!$planet) {
            /** @var Ship[] $targets */
            $targets = $map->getNearbyEntitiesByDistance($ship, false);
            foreach ($targets as $target) {
                if ($target->getOwner() !== $map->getMe()) {
                    $navigate = $ship->navigate($map, $target->getCoordinate(), 7, true, 10, 0.5, $logger);
                    $connection->move($navigate);
                    continue 2;
                }
            }
        }

        if ($ship->canDock($planet)) {
            $logger->log('Ship can dock planet');
            $connection->move($ship->dock($planet->getId()));
        } else {
            $target = $ship->getClosestCoordinateTo($planet);
            $navigate = $ship->navigate($map, $target, 7, true, 10, 0.5, $logger);
            $willCollide = false;
            foreach ($map->getMe()->getShips() as $otherShip) {
                if ($ship === $otherShip || !$otherShip->getCoordinateNextTurn() || !$ship->getCoordinateNextTurn()) {
                    continue;
                }

                $willCollide |= $ship
                    ->getCoordinateNextTurn()
                    ->hasCollision(
                        $ship->getRadius(),
                        $otherShip->getCoordinateNextTurn(),
                        $otherShip->getRadius()
                    )
                ;
            }

            if ($willCollide) {
                $logger->log('will collide stay');
            } else {
                $logger->log('Cannot dock -> navigate to: '.$navigate);
                $connection->move($navigate);
            }

        }
    }
    $connection->flush();
}
