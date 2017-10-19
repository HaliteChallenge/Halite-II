require('should');

const Geometry = require('./Geometry');

describe('Geometry', () => {
    it('distance between two points', () => {
        const distance = Geometry.distance({x: 10, y: 10}, {x: 40, y: -20});
        distance.should.be.approximately(42.4264, 0.0001);
    });

    it('angle in rad between two points should be between 0 and 2Pi', () => {
        expectRad(0, {x: 0, y: 0}, {x: 130, y: 0});
        expectRad(Math.PI / 2, {x: 0, y: 0}, {x: 0, y: 10});
        expectRad(Math.PI, {x: 0, y: 0}, {x: -100, y: 0});
        expectRad(1.5 * Math.PI, {x: 0, y: 0}, {x: 0, y: -100});
        expectRad(1.9999999 * Math.PI, {x: 0, y: 0}, {x: 100000, y: -1});

        function expectRad(expected, start, end) {
            const angleInRad = Geometry.angleInRad(start, end);
            angleInRad.should.be.approximately(expected, 0.00001);
        }
    });

    it('line intersects circle', () => {
        Geometry.intersectSegmentCircle({x: 0, y: 0}, {x: 100, y: 0}, {x: 50, y: -1, radius: 1}).should.be.true();
        Geometry.intersectSegmentCircle({x: 0, y: 0}, {x: 0, y: 100}, {x: 50, y: 50, radius: 1}).should.be.false();
        Geometry.intersectSegmentCircle({x: 0, y: 0}, {x: 100, y: 100}, {x: 50, y: 50, radius: 1}).should.be.true();
        Geometry.intersectSegmentCircle({x: 0, y: 0}, {x: -100, y: -100}, {x: -40, y: -20, radius: 1}, 14).should.be.true();
    });
});