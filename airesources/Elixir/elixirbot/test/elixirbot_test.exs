defmodule ElixirbotTest do
  use ExUnit.Case
  doctest Elixirbot

  test "greets the world" do
    assert Elixirbot.hello() == :world
  end
end
