const Geometry = require('./Geometry');
const Entity = require('./Entity');

const constants = require('./Constants');

const dockingStatus = require('./DockingStatus');

class Ship extends Entity {
    /**
     * @param {GameMap} gameMap map this ship belongs to
     * @param ownerId id of the owner
     * @param params ship information
     */
    constructor(gameMap, ownerId, params) {
        super(params);
        this._gameMap = gameMap;
        this._ownerId = ownerId;

        // adding defaults to simplify game setup for unit testing
        this._params = {
            health: constants.BASE_SHIP_HEALTH,
            dockingStatus: dockingStatus.UNDOCKED,
            ...params }
    }

    isDocked() {
        return this.dockingStatus === dockingStatus.DOCKED;
    }

    isDocking() {
        return this.dockingStatus === dockingStatus.DOCKING;
    }

    isUndocking() {
        return this.dockingStatus === dockingStatus.UNDOCKING;
    }

    isUndocked() {
        return this.dockingStatus === dockingStatus.UNDOCKED;
    }

    /**
     * determines if a planet can be docked
     * @param {Planet} planet
     * @return {boolean|number}
     */
    canDock(planet) {
        return (Geometry.distance(this, planet) <= constants.SHIP_RADIUS + planet.radius + constants.DOCK_RADIUS) &&
            (planet.hasDockingSpot()) &&
            (planet.isFree() || planet.ownerId === this.ownerId);
    }

    get ownerId() {
        return this._ownerId;
    }

    get dockingStatus() {
        return this._params.dockingStatus;
    }

    get radius() {
        return constants.SHIP_RADIUS;
    }

    get dockedPlanetId() {
        return this._params.dockedPlanetId;
    }

    get dockingProgress() {
        return this._params.dockingProgress;
    }

    get weaponCooldown() {
        return this._params.weaponCooldown;
    }

    /**
     * return {x, y} point that is <delta> distance before target
     * @param {{x, y}|Entity} target
     * @param {number} delta
     * @return {{x, y}} new point
     */
    pointApproaching(target, delta) {
        return Geometry.reduceEnd(this, target, delta);
    }

    dock(planet) {
        return `d ${this.id} ${planet.id}`;
    }

    unDock() {
        return `u ${this.id}`;
    }

    thrust(magnitude, angle) {
        return `t ${this.id} ${magnitude | 0} ${angle | 0}`;
    }

    /**
     * Move a ship to a specific target position (Entity). It is recommended to place the position
     * itself here, else navigate will crash into the target. If avoidObstacles is set to true (default)
     * will avoid obstacles on the way, with up to maxCorrections corrections. Note that each correction accounts
     * for angularStep degrees difference, meaning that the algorithm will naively try max_correction degrees before giving
     * up (and returning null). The navigation will only consist of up to one command; call this method again
     * in the next turn to continue navigating to the position.
     * @param {Entity} target the entity to which you will navigate
     * @param {number} keepDistanceToTarget distance to maintain to the target
     * @param {number} speed the (max) speed to navigate. if the obstacle is nearer, will adjust accordingly.
     * @param {boolean} avoidObstacles whether to avoid the obstacles in the way (simple pathfinding).
     * @param {number} maxCorrections the maximum number of degrees to deviate per turn while trying to pathfind. if exceeded returns null.
     * @param {number} angularStep the degree difference to deviate if the original destination has obstacles
     * @param {boolean} ignoreShips whether to ignore ships in calculations (this will make your movement faster, but more precarious)
     * @param {boolean} ignorePlanets whether to ignore planets in calculations (useful if you want to crash onto planets)
     */
    navigate({target, keepDistanceToTarget = 0, speed, avoidObstacles = true, maxCorrections = 90, angularStep = 1,
             ignoreShips = false, ignorePlanets = false}) {
        if (maxCorrections <= 0) {
            return null
        }

        if (avoidObstacles) {
            const obstacles = (ignoreShips && ignorePlanets) ? [] :
                (!ignorePlanets && !ignoreShips) ? this._gameMap.obstaclesBetween(this, target):
                    ignoreShips ? this._gameMap.planetsBetween(this, target) :
                    ignorePlanets ? this._gameMap.shipsBetween(this, target) : [];

            if (obstacles.length) {
                return this.navigate({
                    target: Geometry.rotateEnd(this, target, angularStep),
                    keepDistanceToTarget,
                    speed: speed,
                    avoidObstacles,
                    maxCorrections: maxCorrections - 1,
                    angularStep, ignoreShips, ignorePlanets})
            }
        }

        const closeToTarget = Geometry.reduceEnd(this, target, keepDistanceToTarget);
        const distance = this.distanceBetween(closeToTarget);
        const angleDegree = this.angleBetweenInDegree(closeToTarget);

        const newSpeed = distance >= speed ? speed : distance;
        return this.thrust(newSpeed, angleDegree);
    }

    toString() {
        return 'ship. owner id: ' + this.ownerId + ': ' + JSON.stringify(this._params);
    }
}

module.exports = Ship;