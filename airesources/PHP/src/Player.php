<?php

class Player
{
    /**
     * @var int
     */
    private $id;

    /**
     * @var Ship[]
     */
    private $ships;

    public function __construct(int $id)
    {
        $this->id = $id;
    }

    public function getShip(int $shipId): Ship
    {
        return $this->ships[$shipId];
    }

    public function getId(): int
    {
        return $this->id;
    }

    /**
     * @return Ship[]
     */
    public function getShips(): array
    {
        return $this->ships;
    }

    /**
     * @param Ship[] $ships
     */
    public function setShips(array $ships): void
    {
        $this->ships = $ships;
    }
}
