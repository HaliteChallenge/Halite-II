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


    public function __construct(
        Player $owner = null,
        int $id,
        Coordinate $coordinate,
        int $health,
        float $radius,
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

    public function isDockable(Player $player): bool
    {
        return !$this->isFull() && (!$this->isOwned() || ($this->getOwner() === $player));
    }

    public function isFull(): bool
    {
        return $this->getFreeDockingSpots() <= 0;
    }

    public function getFreeDockingSpots(): int
    {
        return $this->getDockingSpots() - $this->getDockedShips();
    }

    public function getDockedShips(): int
    {
        return $this->dockedShips;
    }

    public function getShip(int $shipId): Ship
    {
        return $this->ships[$shipId];
    }

    /**
     * @return Ship[]
     */
    public function getShips(): array
    {
        return $this->ships;
    }

    public function jsonSerialize(): array
    {
        return array_merge(
            parent::jsonSerialize(),
            [
                'dockingSpots' => $this->getDockingSpots(),
                'production' => $this->getProduction(),
                'isOwned' => $this->isOwned() ? 'yes' : 'no',
                'dockedShips' => $this->getDockedShips(),
                'isFull' => $this->isFull() ? 'yes' : 'no',
                'freeDockingSpots' => $this->getFreeDockingSpots(),
                'isDockableForOwner' => $this->isDockable($this->getOwner()) ? 'yes' : 'no',
                'ships' => array_map(
                    function (Ship $ship) {
                        return $ship->jsonSerialize();
                    },
                    $this->getShips()
                ),
            ]
        );
    }
}
