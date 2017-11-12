class Numeric
  def angle_rad_to_deg_clipped
    (self * 180.0 / Math::PI).round.modulo(360)
  end
end
