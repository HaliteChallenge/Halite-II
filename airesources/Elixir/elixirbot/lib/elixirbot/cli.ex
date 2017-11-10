defmodule Elixirbot.CLI do
  def main(args \\ []) do
    Elixirbot.Game.connect("Alchemist")
      |> Elixirbot.Game.update_map
  end
end
