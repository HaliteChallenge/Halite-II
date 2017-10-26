package hlt

import "math"

// DegToRad ...
func DegToRad(d float64) float64 {
	return d / 180 * math.Pi
}

// RadToDeg ...
func RadToDeg(r float64) float64 {
	return r / math.Pi * 180
}
