<?php

class Entity implements JsonSerializable
{
    /**
     * @var Player|null
     */
    private $owner;

    /**
     * @var int
     */
    private $id;

    /**
     * @var Coordinate
     */
    private $coordinate;

    /**
     * @var int
     */
    private $health;

    /**
     * @var float
     */
    private $radius;

    public function __construct(Player $owner = null, int $id, Coordinate $coordinate, int $health, float $radius)
    {
        $this->owner = $owner;
        $this->id = $id;
        $this->coordinate = $coordinate;
        $this->health = $health;
        $this->radius = $radius;
    }

    /**
     * @return null|Player
     */
    public function getOwner()
    {
        return $this->owner;
    }

    public function getId(): int
    {
        return $this->id;
    }

    public function getCoordinate(): Coordinate
    {
        return $this->coordinate;
    }

    public function getHealth(): int
    {
        return $this->health;
    }

    public function getRadius(): float
    {
        return $this->radius;
    }

    public function getDistanceTo(Entity $target): float
    {
        return $this->getCoordinate()->getDistanceTo($target->getCoordinate());
    }

    public function getAngleTo(Entity $target): float
    {
        return $this->getCoordinate()->getAngleTo($target->getCoordinate());
    }

    public function getClosestCoordinateTo(Entity $target, $space = 3): Coordinate
    {
        $radius = $target->getRadius() + $space;
        $angleRad = $this->getAngleTo($this);

        $x = $target->getCoordinate()->getX() + $radius * cos($angleRad);
        $y = $target->getCoordinate()->getY() + $radius * sin($angleRad);

        return new Coordinate($x, $y);
    }

    public function jsonSerialize(): array
    {
        return [
            'id' => $this->getId(),
            'coordinate' => $this->getCoordinate()->jsonSerialize(),
            'health' => $this->getHealth(),
            'radius' => $this->getRadius(),
        ];
    }
}
