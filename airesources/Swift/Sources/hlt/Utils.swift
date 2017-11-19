import Foundation

public extension Double {
    /// Converts a value in degrees to radians
    public var degreesToRadians: Double {
        return self * .pi / 180.0
    }
    
    /// Converts a value in radians to degrees
    public var radiansToDegrees: Double {
        return self * 180.0 / .pi
    }
}
