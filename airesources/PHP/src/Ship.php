<?php

class Ship extends Entity
{
    /**
     * @var Velocity
     */
    private $velocity;

    /**
     * @var bool
     */
    private $docked;

    /**
     * @var int
     */
    private $planetId;

    /**
     * @var float
     */
    private $dockingProgress;

    /**
     * @var float
     */
    private $weaponCooldown;

    /**
     * @var Planet|null
     */
    private $planet;

    public function __construct(
        Player $owner,
        int $id,
        Coordinate $coordinate,
        int $health,
        Velocity $velocity,
        bool $docked,
        int $planetId,
        float $dockingProgress,
        float $weaponCooldown
    ) {
        parent::__construct($owner, $id, $coordinate, $health, 0.5);
        $this->velocity = $velocity;
        $this->docked = $docked;
        $this->planetId = $planetId;
        $this->dockingProgress = $dockingProgress;
        $this->weaponCooldown = $weaponCooldown;
    }

    public function getVelocity(): Velocity
    {
        return $this->velocity;
    }

    public function isDocked(): bool
    {
        return $this->docked;
    }

    public function getPlanetId(): int
    {
        return $this->planetId;
    }

    public function getDockingProgress(): float
    {
        return $this->dockingProgress;
    }

    public function getWeaponCooldown(): float
    {
        return $this->weaponCooldown;
    }

    /**
     * @return null|Planet
     */
    public function getPlanet()
    {
        return $this->planet;
    }

    public function setPlanet(?Planet $planet): void
    {
        $this->planet = $planet;
    }
}
