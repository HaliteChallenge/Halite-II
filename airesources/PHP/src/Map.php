<?php

class Map
{
    /**
     * @var int
     */
    private $playerId;

    /**
     * @var int
     */
    private $width;

    /**
     * @var int
     */
    private $height;

    /**
     * @var Player[]
     */
    private $players;

    /**
     * @var Ship[]
     */
    private $ships;

    /**
     * @var Planet[]
     */
    private $planets;

    public function __construct(int $playerId, int $width, int $height)
    {
        $this->playerId = $playerId;
        $this->width = $width;
        $this->height = $height;
    }

    public function update(array $players, array $ships, array $planets)
    {
        $this->players = $players;
        $this->ships = $ships;
        $this->planets = $planets;
    }

    public function getMe(): Player
    {
        return $this->getPlayer($this->playerId);
    }

    public function getPlayer(int $playerId): Player
    {
        return $this->players[$playerId];
    }

    public function getWidth(): int
    {
        return $this->width;
    }

    public function getHeight(): int
    {
        return $this->height;
    }

    public function getPlayers(): array
    {
        return $this->players;
    }

    public function getShips(): array
    {
        return $this->ships;
    }

    public function getPlanets(): array
    {
        return $this->planets;
    }

    public function getPlanet(int $planetId): Planet
    {
        return $this->planets[$planetId];
    }

    /**
     * @param Entity $entity
     * @param bool   $planets
     * @param bool   $ships
     *
     * @return Generator|Entity[]
     */
    public function getNearbyEntitiesByDistance(Entity $entity, bool $planets = true, bool $ships = true): \Generator
    {
        $entities = $this->getEntities($planets, $ships);

        usort(
            $entities,
            function (Entity $a, Entity $b) use ($entity) {
                return $a->getDistanceTo($entity) <=> $b->getDistanceTo($entity);
            }
        );

        foreach ($entities as $target) {
            if ($target !== $entity) {
                yield $target;
            }
        }
    }

    /**
     * @param Ship $ship
     *
     * @return Planet|null
     */
    public function getNextDockablePlanet(Ship $ship)
    {
        $planets = $this->getNearbyEntitiesByDistance($ship, true, false);
        foreach ($planets as $planet) {
            /** @var $planet Planet */
            if ($planet->getFreeDockingSpots() > 0) {
                return $planets->current();
            }
        }

        return null;
    }

    /**
     * @param Ship       $ship
     * @param Coordinate $coordinate
     * @param Entity     $target
     * @param bool       $planets
     * @param bool       $ships
     *
     * @return Generator
     */
    public function getEntitiesBetween(
        Ship $ship,
        Coordinate $coordinate,
        Entity $target = null,
        bool $planets = true,
        bool $ships = true
    ): \Generator {
        foreach ($this->getEntities($planets, $ships) as $entity) {
            if ($entity === $ship || $entity === $target) {
                continue;
            }

            if ($this->hasCollision($ship, $coordinate, $entity)) {
                yield $entity;
            }
        }
    }

    /**
     * @param Ship       $ship
     * @param Coordinate $target
     * @param Entity     $entity
     *
     * @return bool
     */
    private function hasCollision(Ship $ship, Coordinate $target, Entity $entity): bool
    {
        $fudge = $ship->getRadius() + 0.1;
        $circleRadius = $entity->getRadius();

        $startX = $ship->getCoordinate()->getX();
        $startY = $ship->getCoordinate()->getY();
        $endX = $target->getX();
        $endY = $target->getY();
        $centerX = $entity->getCoordinate()->getX();
        $centerY = $entity->getCoordinate()->getY();

        $dx = $endX - $startX;
        $dy = $endY - $startY;

        $a = $dx ** 2 + $dy ** 2;
        $b = -2 * (($startX ** 2) - ($startX * $endX)
                - ($startX * $centerX) + ($endX * $centerX)
                + ($startY ** 2) - ($startY * $endY)
                - ($startY * $centerY) + ($endY * $centerY));

        if ($a === 0.0) {
            return $ship->getDistanceTo($entity) <= $circleRadius + $fudge;
        }

        $t = min(-$b / (2 * $a), 1.0);
        if ($t < 0) {
            return false;
        }

        $closestX = $startX + $dx * $t;
        $closestY = $startY + $dy * $t;

        $closestEntity = new Entity(null, 0, new Coordinate($closestX, $closestY), 0, 0);
        $closestDistance = $closestEntity->getDistanceTo($entity);


        return $closestDistance <= $circleRadius + $fudge;
    }

    /**
     * @param bool $planets
     * @param bool $ships
     *
     * @return Entity[]
     */
    private function getEntities(bool $planets, bool $ships): array
    {
        $entities = [];
        if ($planets) {
            $entities = array_merge($entities, $this->planets);
        }
        if ($ships) {
            $entities = array_merge($entities, $this->ships);
        }

        return $entities;
    }
}
