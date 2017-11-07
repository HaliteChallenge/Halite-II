val deg2rad : float -> float
val rad2deg : float -> float
val pi : float
val update_map : Td.game_state -> unit
val init : Td.game_state -> string -> (Td.game_state -> 'a) -> unit
val send_command_queue : Td.move list -> unit
val get_me : Td.game_state -> Td.player
val get_player : Td.game_state -> int -> Td.player
val all_players : Td.game_state -> Td.player array
val get_planet : Td.game_state -> int -> Td.planet
val all_planets : Td.game_state -> Td.planet list
val planet_entities : Td.planet list -> Td.entity list
val ship_entities : Td.ship list -> Td.entity list
val all_ship_entities : Td.game_state -> Td.entity list
val all_entities : Td.game_state -> Td.entity list
val all_entities_except : Td.game_state -> Td.entity -> Td.entity list
val calculate_distance_between : Td.entity -> Td.entity -> float
val calculate_radians_between : Td.entity -> Td.entity -> float
val calculate_angle_between : Td.entity -> Td.entity -> float
val closest_point : Td.entity -> Td.entity -> float -> float * float
val nearby_entities_by_distance :
  Td.game_state -> Td.entity -> (Td.entity * float) list
val all_ships : Td.game_state -> Td.ship list
val docked_ships : Td.planet -> Td.ship list
val get_planet_docked_ship : Td.planet -> int -> Td.ship option
val planet_is_owned : Td.planet -> bool
val planet_is_full : Td.planet -> bool
val positive_int_degrees : int -> int
val cmd_thrust : Td.ship -> int -> float -> [> `Thrust of int * int * int ]
val cmd_dock : Td.ship -> Td.planet -> [> `Dock of int * int ]
val cmd_undock : Td.ship -> [> `Undock of int ]
val can_dock : Td.ship -> Td.planet -> bool
val position_entity : float -> float -> Td.entity
val intersect_segment_circle :
  Td.entity -> Td.entity -> Td.entity -> float -> bool
val obstacles_between :
  Td.entity -> Td.entity -> Td.entity list -> Td.entity list
val direct_navigate :
  Td.ship -> float -> int -> float -> [> `Thrust of int * int * int ]
val navigate :
  Td.game_state ->
  Td.ship ->
  Td.entity ->
  int ->
  bool ->
  int -> float -> bool -> bool -> [> `Thrust of int * int * int ] option
