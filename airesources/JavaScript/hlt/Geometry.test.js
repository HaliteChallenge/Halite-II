require('should');

const Geometry = require('./Geometry');

describe('Geometry', () => {
    it('distance between two points', () => {
        const distance = Geometry.distance({x: 10, y: 10}, {x: 40, y: 40});
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

    it('angle in degree between two points should be between 0 and 2Pi', () => {
        expectDegree(0, {x: 0, y: 0}, {x: 130, y: 0});
        expectDegree(90, {x: 0, y: 0}, {x: 0, y: 10});
        expectDegree(180, {x: 100, y: 0}, {x: 10, y: 0});
        expectDegree(270, {x: 0, y: 100}, {x: 0, y: 10});
        expectDegree(359.99, {x: 0, y: 1}, {x: 100000, y: 0});

        function expectDegree(expected, start, end) {
            const angleInRad = Geometry.angleInDegree(start, end);
            angleInRad.should.be.approximately(expected, 0.01);
        }
    });

    it('new position by rotation', () => {
        rotateAndValidateEnd({x: 10, y: 10}, {x: 30, y: 30}, 0, {x: 30, y: 30});
        rotateAndValidateEnd({x: 30, y: 30}, {x: 10, y: 10}, 0, {x: 10, y: 10});
        rotateAndValidateEnd({x: 10, y: 10}, {x: 30, y: 30}, 1, {x: 29.64, y: 30.34});
        rotateAndValidateEnd({x: 10, y: 10}, {x: 30, y: 30}, -1, {x: 30.34, y: 29.64});

        function rotateAndValidateEnd(start, end, angleRotation, expected) {
            const newEnd = Geometry.rotateEnd(start, end, angleRotation);
            newEnd.x.should.be.approximately(expected.x, 0.01);
            newEnd.y.should.be.approximately(expected.y, 0.01);
        }
    });

    it('new position by reducing', () => {
        reduceAndValidateEnd({x: 10, y: 10}, {x: 30, y: 30}, 0, {x: 30, y: 30});
        reduceAndValidateEnd({x: 30, y: 30}, {x: 10, y: 10}, 0, {x: 10, y: 10});
        reduceAndValidateEnd({x: 10, y: 10}, {x: 30, y: 30}, 3, {x: 27.88, y: 27.88});
        reduceAndValidateEnd({x: 30, y: 30}, {x: 10, y: 10}, 3, {x: 12.12, y: 12.12});

        function reduceAndValidateEnd(start, end, delta, expected) {
            const newEnd = Geometry.reduceEnd(start, end, delta);
            newEnd.x.should.be.approximately(expected.x, 0.01);
            newEnd.y.should.be.approximately(expected.y, 0.01);
        }
    });

    it('line intersects circle', () => {
        Geometry.intersectSegmentCircle({x: 0, y: 0}, {x: 100, y: 0}, {x: 50, y: -1, radius: 1}, 0.5).should.be.true();
        Geometry.intersectSegmentCircle({x: 0, y: 0}, {x: 0, y: 100}, {x: 50, y: 50, radius: 1}, 0.5).should.be.false();
        Geometry.intersectSegmentCircle({x: 0, y: 0}, {x: 100, y: 100}, {x: 50, y: 50, radius: 1}, 0.5).should.be.true();
        Geometry.intersectSegmentCircle({x: 0, y: 0}, {x: -100, y: -100}, {x: -40, y: -20, radius: 1}, 14).should.be.true();
    });
});