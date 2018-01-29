defmodule Elixirbot.CLI do
  def main(_ \\ []) do
    Elixirbot.Game.connect("Alchemist")
      |> Elixirbot.Game.run
  end
end
