package hlt

import "math"

// DegToRad converts degrees to radians
func DegToRad(d float64) float64 {
	return d / 180 * math.Pi
}

// RadToDeg converts radians to degrees
func RadToDeg(r float64) float64 {
	return r / math.Pi * 180
}
