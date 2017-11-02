<?php

class Planet extends Entity
{
    /**
     * @var int
     */
    private $dockingSpots;

    /**
     * @var int
     */
    private $production;

    /**
     * @var int
     */
    private $remainingProduction;

    /**
     * @var bool
     */
    private $isOwned;

    /**
     * @var int
     */
    private $dockedShips;

    /**
     * @var Ship[]
     */
    private $ships;

    /**
     * @param Player|null $owner
     * @param int         $id
     * @param Coordinate  $coordinate
     * @param int         $health
     * @param int         $radius
     * @param int         $dockingSpots
     * @param int         $production
     * @param int         $remainingProduction
     * @param bool        $isOwned
     * @param int         $dockedShips
     * @param Ship[]      $ships
     */
    public function __construct(
        ?Player $owner,
        int $id,
        Coordinate $coordinate,
        int $health,
        int $radius,
        int $dockingSpots,
        int $production,
        int $remainingProduction,
        bool $isOwned,
        int $dockedShips,
        array $ships
    ) {
        parent::__construct($owner, $id, $coordinate, $health, $radius);
        $this->dockingSpots = $dockingSpots;
        $this->production = $production;
        $this->remainingProduction = $remainingProduction;
        $this->isOwned = $isOwned;
        $this->dockedShips = $dockedShips;
        $this->ships = $ships;
    }

    public function getDockingSpots(): int
    {
        return $this->dockingSpots;
    }

    public function getProduction(): int
    {
        return $this->production;
    }

    public function getRemainingProduction(): int
    {
        return $this->remainingProduction;
    }

    public function isOwned(): bool
    {
        return $this->isOwned;
    }

    public function getDockedShips(): int
    {
        return $this->dockedShips;
    }

    /**
     * @return Ship[]
     */
    public function getShips(): array
    {
        return $this->ships;
    }
}
