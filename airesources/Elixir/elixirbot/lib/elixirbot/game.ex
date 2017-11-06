defmodule Elixirbot.Game do
  require Logger
  require Map

  defmodule GameMap do
    defstruct player_id: nil, width: nil, height: nil
  end

  defmodule Player do
    defstruct player_id: nil, ships: []
  end

  defmodule Ship do
    defstruct id: nil, owner: nil, x: nil, y: nil, radius: nil, health: nil, docking_status: nil, docking_progress: nil, planet: nil
  end

  defmodule Planet do
    defstruct id: nil, owner: nil, x: nil, y: nil, radius: nil, health: nil, docking_spots: nil, docked_ship_ids: nil, docked_ships: nil
  end

  def connect(name) do
    player_id = read_from_input()
    set_up_logging(name, player_id)
    [width, height] = read_ints_from_input
    Logger.info "game size: #{width} x #{height}"
    write_to_output(name)
    %GameMap{player_id: parse_int(player_id), width: width, height: height}
  end

  defp read_from_input() do
    input_line = IO.gets("") |> String.strip
    Logger.debug input_line
    input_line
  end

  defp set_up_logging(name, player_id) do
    Logger.add_backend {LoggerFileBackend, :debug}
    Logger.configure_backend {LoggerFileBackend, :debug},
      path: "#{player_id}_#{name}.log"
    Logger.info "Starting new game for #{name}:#{player_id}"
    Logger.info "Setting up logger"
  end

  def read_ints_from_input do
    read_from_input()
      |> String.split(" ")
      |> Enum.map(fn(x) -> parse_int(x) end)
  end

  defp write_to_output(message) do
    message = "#{String.strip(message)}\n"
    Logger.info("Sending: #{message}")
    IO.puts(message)
  end

  def update_map(map) do
    tokens = read_from_input() |> String.split(" ")
    {tokens, players} = read_from_input() |> String.split(" ") |> parse_players
    {[], planets} = tokens |> parse_planets
    make_move(map, players, planets)
      |> Enum.join(" ")
      |> write_to_output
    update_map(map)
  end

  def make_move(map, players, planets) do
    Logger.info("finding player #{map.player_id}")
    player = Enum.find(players, fn(player) -> Logger.info("finding player #{inspect(player)}, #{inspect(player.player_id)} == #{inspect(map.player_id)} = #{player.player_id == map.player_id}"); player.player_id == map.player_id end)
    Logger.info("found player #{inspect(player)}")
    opponent_ships = Enum.flat_map(players, fn(player) -> if (player.player_id == map.player_id), do: [], else: player.ships end)
    Enum.map(player.ships, fn(ship) -> make_move_for_ship(ship, opponent_ships, planets) end)
  end

  def make_move_for_ship(ship, opponent_ships, planets) do
    Logger.info("making move for ship: #{inspect(ship)}")
    "t #{ship.id} #{Enum.random(1..7)} #{Enum.random(0..359)}"
  end

  def parse_players(tokens) do
    [count_of_players|tokens] = tokens
    Logger.info "Parsing #{count_of_players} players"

    {players, tokens} = parse_players(parse_int(count_of_players), tokens)

    Logger.debug "Parsed players: #{inspect(players)}"
    {tokens, players}
  end

  def parse_players(0, tokens), do: {[], tokens}
  def parse_players(count_of_players, tokens) do
    [player_id|tokens] = tokens
    player_id = parse_int(player_id)
    {ships, tokens} = parse_ships(player_id, tokens)

    player = %Player{player_id: player_id, ships: ships}
    Logger.debug "Parsed player: #{inspect(player)}"

    {players, tokens} = parse_players(count_of_players-1, tokens)
    {players++[player], tokens}
  end

  def parse_ships(player_id, tokens) do
    [count_of_ships|tokens] = tokens

    {ships, tokens} = parse_ships(parse_int(count_of_ships), player_id, tokens)
    Logger.debug "Parsed ships: #{inspect(ships)}"

    {ships, tokens}
  end

  def parse_ships(0, _, tokens), do: {[], tokens}
  def parse_ships(count_of_ships, player_id, tokens) do
    [id, x, y, hp, _, _, status, planet, progress, _|tokens] = tokens
    id = parse_int(id)

    ship = %Ship{id: id,
                 owner: player_id,
                 x: parse_float(x),
                 y: parse_float(y),
                 radius: nil,
                 health: parse_int(hp),
                 docking_status: parse_int(status),
                 docking_progress: parse_int(progress),
                 planet: parse_int(planet)}
    Logger.debug "Parsed ship: #{inspect(ship)}"

    {ships, tokens} = parse_ships(count_of_ships-1, player_id, tokens)
    {ships++[ship], tokens}
  end

  def parse_planets(tokens) do
    [count_of_planets|tokens] = tokens
    planets = %{}

    {planets, tokens} = parse_planets(parse_int(count_of_planets), tokens)

    Logger.debug "Parsed planets: #{inspect(planets)}"
    {tokens, planets}
  end

  def parse_planets(0, tokens), do: {%{}, tokens}
  def parse_planets(count_of_planets, tokens) do
    [id, x, y, hp, r, docking_spots, _, _, is_owned, owner, ship_count|tokens] = tokens
    id = parse_int(id)
    owner = parse_int(owner)

    # Fetch the ship ids from the tokens array
    docked_ships = parse_docked_ships(parse_int(ship_count), tokens)

    planet = %Planet{id: id,
                     x: parse_float(x),
                     y: parse_float(y),
                     health: parse_int(hp),
                     radius: parse_float(r),
                     docking_spots: parse_int(docking_spots),
                     owner: owner,
                     docked_ships: docked_ships}
    Logger.debug "Parsed planet: #{inspect(planet)}"

    {planets, tokens} = parse_planets(count_of_planets-1, tokens)
    {Map.merge(%{id => planet}, planets), tokens}
  end

  defp parse_docked_ships(0, tokens), do: {[], tokens}
  defp parse_docked_ships(ship_count, tokens) do
    [ship|tokens] = tokens
    {ships, tokens} = parse_docked_ships(ship_count-1, tokens)
    {ships++[ship], tokens}
  end

  defp parse_int(nil), do: nil
  defp parse_int(str) do
    str |> String.to_integer
  end

  defp parse_float(nil), do: nil
  defp parse_float(str) do
    str |> String.to_float
  end
end
