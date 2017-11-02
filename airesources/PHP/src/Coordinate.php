<?php

class Coordinate
{
    /**
     * @var float
     */
    private $x;

    /**
     * @var float
     */
    private $y;

    public function __construct(float $x, float $y)
    {
        $this->x = $x;
        $this->y = $y;
    }

    public function getX(): float
    {
        return $this->x;
    }

    public function getY(): float
    {
        return $this->y;
    }

    public function getDistanceTo(Coordinate $target): float
    {
        $dx = $this->getX() - $target->getX();
        $dy = $this->getY() - $target->getY();

        return sqrt(($dx ** 2) + ($dy ** 2));
    }

    public function getAngleTo(Coordinate $target): float
    {
        $dx = $target->getX();
        $dy = $target->getY();

        return atan2($dy, $dx) + 2 * M_PI;
    }
}
