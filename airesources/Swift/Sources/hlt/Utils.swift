import Foundation

public extension FloatingPoint {
    /// Converts a value in degrees to radians
    public var degreesToRadians: Self {
        return self * .pi / 180
    }
    
    /// Converts a value in radians to degrees
    public var radiansToDegrees: Self {
        return self * 180 / .pi
    }
}
