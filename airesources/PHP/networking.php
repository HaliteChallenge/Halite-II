<?php

$_productions = [];
$_width = -1;
$_height = -1;

function serializeMoveSet(array $moves)
{
    $returnString = '';
    foreach ($moves as $move) {
        $returnString .= $move->loc->x . ' ' . $move->loc->y . ' ' . $move->direction . ' ';
    }
    return $returnString;
}

function deserializeMapSize($inputString)
{
    global $_width, $_height;

    $splitString = explode(' ', $inputString);

    $_width = (int)array_shift($splitString);
    $_height = (int)array_shift($splitString);
}

function deserializeProductions($inputString)
{
    global $_width, $_height, $_productions;

    $splitString = explode(' ', $inputString);

    for ($a = 0; $a < $_height; ++$a) {
        $row = [];
        for ($b = 0; $b < $_width; ++$b) {
            $row[] = (int)array_shift($splitString);
        }
        $_productions[] = $row;
    }
}

function deserializeMap($inputString)
{
    global $_width, $_height, $_productions;

    $splitString = explode(' ', $inputString);

    $m = new GameMap($_width, $_height);

    $y = 0;
    $x = 0;
    while ($y !== $m->height) {
        $counter = (int)array_shift($splitString);
        $owner = (int)array_shift($splitString);
        for ($a = 0; $a < $counter; ++$a) {
            $m->contents[$y][$x]->owner = $owner;
            $x += 1;
            if ($x === $m->width) {
                $x = 0;
                $y += 1;
            }
        }
    }

    for ($a = 0; $a < $_height; ++$a) {
        for ($b = 0; $b < $_width; ++$b) {
            $m->contents[$a][$b]->strength = (int)array_shift($splitString);
            $m->contents[$a][$b]->production = $_productions[$a][$b];
        }
    }

    return $m;
}

function sendString($toBeSent)
{
    $toBeSent .= "\n";

    fwrite(STDOUT, $toBeSent);
}

function getString()
{
    $input = fgets(STDIN);

    if ($input === false) {
        exit;
    }

    return rtrim($input, "\n");
}


function getInit()
{
    $playerTag = (int)getString();
    deserializeMapSize(getString());
    deserializeProductions(getString());
    $m = deserializeMap(getString());

    return [$playerTag, $m];
}

function sendInit($name)
{
    sendString($name);
}

function getFrame()
{
    return deserializeMap(getString());
}

function sendFrame(array $moves)
{
    sendString(serializeMoveSet($moves));
}
