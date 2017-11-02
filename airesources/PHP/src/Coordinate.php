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
        $dx = $target->getX() - $this->getX();
        $dy = $target->getY() - $this->getY();

        $result = atan2($dy, $dx);
        if($result < 0 ){
            $result += 2 * M_PI;
        }
        return $result;
    }


}
