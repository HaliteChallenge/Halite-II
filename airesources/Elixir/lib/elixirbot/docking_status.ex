defmodule DockingStatus do
  def undocked,  do: 0
  def docking,   do: 1
  def docked,    do: 2
  def undocking, do: 3
  def all,       do: [undocked(), docking(), docked(), undocking()]
end
