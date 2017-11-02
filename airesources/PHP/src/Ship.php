<?php

class Ship extends Entity
{
    const UNDOCK_KEY = 'u';
    const DOCK_KEY = 'd';
    const THRUST_KEY = 't';

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

    public function thrust(int $thrust, float $angle): string
    {
        return self::THRUST_KEY.' '.$this->getId().' '.$thrust.' '.round($angle);
    }

    public function dock(int $planetId): string
    {
        return self::DOCK_KEY.' '.$this->getId().' '.$planetId;
    }

    public function undock(): string
    {
        return self::UNDOCK_KEY.' '.$this->getId();
    }

    public function canDock(Planet $planet): bool
    {
        return $planet->isDockable($this->getOwner()) &&
            $planet->getCoordinate()->getDistanceTo($this->getCoordinate()) <= $planet->getRadius() + 4;
    }

    public function navigate(
        Map $map,
        Coordinate $target,
        int $thrust,
        bool $avoidObstacles,
        int $maxCorrections,
        float $angularStepRad,
        Logger $logger
    ): string {
        $logger->log(
            'Navigate from '.print_r($this->getCoordinate(), true).' to '.print_r($target, true).' / max corrections '.$maxCorrections
        );

        if ($maxCorrections <= 0) {
            return '';
        }
        $distance = $this->getCoordinate()->getDistanceTo($target);
        $angleRad = $this->getCoordinate()->getAngleTo($target);

        $hasObstacles = (bool) $map->getEntitiesBetween($this, $target);
        if ($avoidObstacles && $hasObstacles) {
            $newTargetDx = cos($angleRad + $angularStepRad) * $distance;
            $newTargetDy = sin($angleRad + $angularStepRad) * $distance;
            $newTarget = new Coordinate($this->getCoordinate()->getX() + $newTargetDx, $this->getCoordinate()->getY() + $newTargetDy);

            $logger->log('Has Obstacles, correct position to: '.print_r($newTarget, true));

            return $this->navigate($map, $newTarget, $thrust, $avoidObstacles, $maxCorrections - 1, $angularStepRad, $logger);
        }

        $computedThrust = 7;
        if ($distance < $thrust) {
            $computedThrust = (int) $distance;
        }

        //$angleDeg = self::angleRadToDegClipped($angleRad);
        $logger->log('Distance: '.$distance.' / AngleRad: '.$angleRad.' / AngleDeg: '.$angleRad);

        return $this->thrust($computedThrust, $angleRad);
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

    private static function angleRadToDegClipped(float $angleRad): int
    {
        $degUnclipped = round(rad2deg($angleRad));

        return (int) ((($degUnclipped % 360) + 360) % 360);
    }
}
