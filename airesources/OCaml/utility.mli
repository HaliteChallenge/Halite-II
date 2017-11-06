val time_seconds_elapsed_this_turn : Td.game_state -> float
val new_ship :
  int ->
  int ->
  float ->
  float ->
  int -> 'a -> 'b -> Td.docking_status -> int -> int -> int -> Td.ship
val new_planet :
  int ->
  float ->
  float ->
  int -> float -> int -> int -> int -> bool -> int -> int list -> Td.planet
val init_state : unit -> Td.game_state
