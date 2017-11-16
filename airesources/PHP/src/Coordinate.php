<?php

class Coordinate implements JsonSerializable
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
        if ($result < 0) {
            $result += 2 * M_PI;
        }

        return $result;
    }

    public function jsonSerialize(): array
    {
        return [
            'x' => $this->getX(),
            'y' => $this->getY(),
        ];
    }

    public function forecastMove(int $computedThrust, int $angleDeg)
    {
        $y = $computedThrust * sin(deg2rad($angleDeg));
        $x = $computedThrust * cos(deg2rad($angleDeg));

        return new Coordinate($this->x + $x, $this->y + $y);
    }

    public function hasCollision(float $myRadius, Coordinate $target, float $targetRadius, $padding = 0.3)
    {
        $distance = $this->getDistanceTo($target);
        return ($myRadius + $targetRadius + $padding) > $distance;
    }

    public static function angleRadToDegClipped(float $angleRad): int
    {
        $degUnclipped = round(rad2deg($angleRad));

        return (int) ((($degUnclipped % 360) + 360) % 360);
    }
}
