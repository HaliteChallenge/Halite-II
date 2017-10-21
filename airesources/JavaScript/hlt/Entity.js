class Entity {
    constructor(params) {
        this._params = params;
    }

    get id() {
        return this._params.id;
    }

    get x() {
        return this._params.xPos;
    }

    get y() {
        return this._params.yPos;
    }

    get radius() {
        return this._params.radius;
    }

    get health() {
        return this._params.health;
    }
}