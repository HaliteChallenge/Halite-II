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
     * @param start object with {x, y} properties
     * @param end object with {x, y} properties
     * @returns {number} radian between 0 and 2*PI
     */
    static angleInRad(start, end) {
        const dx = end.x - start.x;
        const dy = end.y - start.y;

        const atan = Math.atan2(dy, dx);
        return atan >= 0 ? atan : (atan + 2 * Math.PI);
    }

    /**
     * Test whether a line segment and circle intersect.
     * @param {object} start object with {x, y} properties
     * @param {object} end object with {x, y} properties
     * @param {object} circle object with {x, y, radius} properties
     * @param {number} [fudge] fudge factor: additional distance to leave between the segment and circle. defaults to 0.5
     * @returns {boolean} true if intersects, false - otherwise
     */
    static intersectSegmentCircle(start, end, circle, fudge) {
        fudge = typeof fudge === 'undefined' ? 0.5 : fudge;

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