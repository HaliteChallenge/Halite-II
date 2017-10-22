class Geometry {
    /**
     * distance between two points
     * @param start object with {x, y} properties
     * @param end object with {x, y} properties
     * @returns {number} distance
     */
    static distance(start, end) {
        const dx = end.x - start.x;
        const dy = end.y - start.y;

        return Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2));
    }

    /**
     * angle in rad between two points
     * @param {object} start object with {x, y} properties
     * @param {object} end object with {x, y} properties
     * @returns {number} radian between 0 and 2*PI
     */
    static angleInRad(start, end) {
        const dx = end.x - start.x;
        const dy = end.y - start.y;

        const atan = Math.atan2(dy, dx);
        return atan >= 0 ? atan : (atan + 2 * Math.PI);
    }

    /**
     * angle in degree between two points
     * @param {object} start object with {x, y} properties
     * @param {object} end object with {x, y} properties
     * @returns {number} angle between 0 and 360
     */
    static angleInDegree(start, end) {
        return Geometry.toDegree(this.angleInRad(start, end));
    }

    /**
     * given start and end positions, adjust end position by rotating line by specified degree
     * @param {object} start object with {x, y} properties
     * @param {object} end object with {x, y} properties
     * @param degreeDelta delta in degree
     */
    static rotateEnd(start, end, degreeDelta) {
        const distance = Geometry.distance(start, end);
        const angleDegree = Geometry.angleInDegree(start, end);

        const newAngleDegree = angleDegree + degreeDelta;
        const newAngleRad = Geometry.toRad(newAngleDegree);
        const x = Math.cos(newAngleRad) * distance;
        const y = Math.sin(newAngleRad) * distance;

        return {x: start.x + x, y: start.y + y};
    }

    /**
     * find the closest point to the given end position near the given target, outside its given radius,
     * with an added fudge of min_distance.
     * @param start {object} start object with {x, y} properties
     * @param end {object} end object with {x, y} properties
     * @param delta distance by what reduce the end position
     */
    static reduceEnd(start, end, delta) {
        const angleRad = Geometry.angleInRad(start, end);

        const dx = Math.cos(angleRad) * delta;
        const dy = Math.sin(angleRad) * delta;

        return {x: end.x - dx, y: end.y - dy};
    }

    /**
     * converts rad to degree
     * @param {number} rad
     * @return {number} degree
     */
    static toDegree(rad) {
        return rad * 180.0 / Math.PI;
    }

    /**
     * converts degree to rad
     * @param {number} degree
     * @return {number} rad
     */
    static toRad(degree) {
        return degree * Math.PI / 180.0;
    }

    /**
     * Test whether a line segment and circle intersect.
     * @param {object} start object with {x, y} properties
     * @param {object} end object with {x, y} properties
     * @param {object} circle object with {x, y, radius} properties
     * @param {number} [fudge] fudge factor: additional distance to leave between the segment and circle
     * @returns {boolean} true if intersects, false - otherwise
     */
    static intersectSegmentCircle(start, end, circle, fudge) {
        const dx = end.x - start.x;
        const dy = end.y - start.y;

        const a = dx ** 2 + dy ** 2;

        if (a === 0.0) {
            return Geometry.distance(start, end) <= (circle.radius + fudge);
        }

        const b = -2 * (start.x ** 2 - start.x * end.x - start.x * circle.x + end.x * circle.x +
            start.y ** 2 - start.y * end.y - start.y * circle.y + end.y * circle.y);

        const t = Math.min(-b / (2 * a), 1.0);
        if (t < 0) {
            return false;
        }

        const closestX = start.x + dx * t;
        const closestY = start.y + dy * t;
        const closestDistance = Geometry.distance({x: closestX, y: closestY}, circle);

        return closestDistance <= circle.radius + fudge;
    }
}

module.exports = Geometry;