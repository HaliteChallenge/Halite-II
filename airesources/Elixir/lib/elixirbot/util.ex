defmodule Elixirbot.Util do
  @rad_in_deg (180/:math.pi)

  @doc """
  Converts radians to degrees

  ## Examples
      iex>Elixirbot.Util.angle_rad_to_deg(:math.pi)
      180.0
  """
  def angle_rad_to_deg(angle_rad) do
    angle_rad * @rad_in_deg
  end

  @doc """
  Converts radians to degrees and ensure between 0 and 360

  ## Examples
      iex>Elixirbot.Util.angle_rad_to_deg_clipped(:math.pi)
      180
      iex>Elixirbot.Util.angle_rad_to_deg_clipped(9)
      156
  """
  def angle_rad_to_deg_clipped(angle_rad) do
    (angle_rad_to_deg(angle_rad) |> round |> rem(360)) + 360 |> rem(360)
  end

  def parse_int(nil), do: nil
  def parse_int(str) do
    str |> String.to_integer
  end

  def parse_float(nil), do: nil
  def parse_float(str) do
    str |> String.to_float
  end
end
