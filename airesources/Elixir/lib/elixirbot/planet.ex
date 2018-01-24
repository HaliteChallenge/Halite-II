# A planet on the game map.
#
# id: The planet ID.
# x: The planet x-coordinate.
# y: The planet y-coordinate.
# radius: The planet radius.
# num_docking_spots: The max number of ships that can be docked.
# health: The planet's health.
# owner: The player ID of the owner, if any. If nil, Entity is not owned.
defmodule Planet do
  import Elixirbot.Util

  defstruct id: nil, owner: nil, x: nil, y: nil, radius: nil, health: nil, num_docking_spots: nil, docked_ships: nil

  def get_docked_ship(planet, id) do
    Enum.find(all_docked_ships(planet), fn(ship) ->
      ship.id == id
    end)
  end

  def all_docked_ships(planet) do
    { ships, _ } = planet.docked_ships
    ships
  end

  def is_owned?(planet) do
    planet.owner != nil
  end

  def is_full?(planet) do
    length(all_docked_ships(planet)) >= planet.num_docking_spots
  end

  def parse(tokens, ships) do
    [count_of_planets|tokens] = tokens

    {planets, tokens} = parse(parse_int(count_of_planets), tokens, ships)

    {tokens, planets}
  end

  def parse(0, tokens, _), do: {%{}, tokens}
  def parse(count_of_planets, tokens, ships) do
    [id, x, y, hp, r, docking_spots, _, _, owned, owner, ship_count|tokens] = tokens

    # Fetch the ship ids from the tokens array
    {docked_ship_ids, tokens} = parse_docked_ship_ids(parse_int(ship_count), tokens)

    planet = %Planet{
      id:                parse_int(id),
      x:                 parse_float(x),
      y:                 parse_float(y),
      health:            parse_int(hp),
      radius:            parse_float(r),
      num_docking_spots: parse_int(docking_spots),
      owner:             if(parse_int(owned) == 0, do: nil, else: parse_int(owner)),
      docked_ships:      Enum.map(docked_ship_ids, fn(ship_id) ->
        Ship.get(ships, ship_id)
      end)
    }

    {planets, tokens} = parse(count_of_planets - 1, tokens, ships)
    {Map.merge(%{id => planet}, planets), tokens}
  end

  defp parse_docked_ship_ids(0, tokens), do: {[], tokens}
  defp parse_docked_ship_ids(ship_count, tokens) do
    [ship_id|tokens] = tokens
    {ship_ids, tokens} = parse_docked_ship_ids(ship_count - 1, tokens)
    {ship_ids++[ship_id], tokens}
  end
end
