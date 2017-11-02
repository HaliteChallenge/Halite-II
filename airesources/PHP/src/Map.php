<?php

class Map
{
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

    public function __construct(int $width, int $height)
    {
        $this->width = $width;
        $this->height = $height;
    }

    public function update(array $players, array $ships, array $planets): void
    {
        $this->players = $players;
        $this->ships = $ships;
        $this->planets = $planets;
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
}
