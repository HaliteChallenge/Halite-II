<?php

require_once 'hlt.php';
require_once 'networking.php';

list($myID, $gameMap) = getInit();
sendInit('myPHPBot');

while (true) {
    $moves = [];
    $gameMap = getFrame();
    for ($y = 0; $y < $gameMap->height; ++$y) {
        for ($x = 0; $x < $gameMap->width; ++$x) {
            if ($gameMap->getSite(new Location($x, $y))->owner === $myID) {
                $moves[] = new Move(new Location($x, $y), rand(0, 4));
            }
        }
    }
    sendFrame($moves);
}
