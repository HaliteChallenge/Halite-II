const Geometry = require('./Geometry');

class Entity {
    constructor(params) {
        this._params = params;
    }

    get id() {
        return this._params.id;
    }

    get x() {
        return this._params.x;
    }

    get y() {
        return this._params.y;
    }

    get radius() {
        return this._params.radius;
    }

    get health() {
        return this._params.health;
    }

    /**
     * calculates the distance between this object and the target
     * @param {Entity} target target
     * @return {number} distance
     */
    distanceBetween(target) {
        return Geometry.distance(this, target);
    }

    /**
     * the angle in degrees between this object and the target in degrees
     * @param {Entity} target target
     * @return {number} angle in degrees
     */
    angleBetweenInDegree(target) {
        return Geometry.angleInDegree(this, target);
    }
}

module.exports = Entity;