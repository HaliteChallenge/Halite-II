<?php

require_once __DIR__.'/src/bootstrap.php';

$connection = new Connection();
$game = new Game('Terminator', $connection);
$turn = 0;

//60s time for game initialization;
$game->sendBotName();

while (true) {
    $turn++;
    Logger::log('--------- NEW TURN (#'.$turn.') ------------');
    $map = $game->updateMap();

    foreach ($map->getMe()->getShips() as $ship) {
        Logger::log('New Turn with ship '.json_encode($ship));
        if ($ship->isDocked() || $ship->getDockingProgress() > 0) {
            continue;
        }
        $planet = $map->getNextDockablePlanet($ship);

        if (!$planet) {
            /** @var Ship[] $targets */
            $targets = $map->getNearbyEntitiesByDistance($ship, false);
            foreach ($targets as $target) {
                if ($target->getOwner() !== $map->getMe()) {
                    $navigate = $ship->navigate($map, $target->getCoordinate(), 7, true, 10, 0.5);
                    $connection->move($navigate);
                    continue 2;
                }
            }
        }

        if ($ship->canDock($planet)) {
            Logger::log('Ship can dock planet');
            $connection->move($ship->dock($planet->getId()));
        } else {
            $target = $ship->getClosestCoordinateTo($planet);
            $navigate = $ship->navigate($map, $target, 7, true, 10, 0.5);
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
                Logger::log('will collide stay');
            } else {
                Logger::log('Cannot dock -> navigate to: '.$navigate);
                $connection->move($navigate);
            }

        }
    }
    $connection->flush();
}
