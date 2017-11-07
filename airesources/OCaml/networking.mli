val get_string : unit -> string
val set_width_height : Td.game_state -> int -> int -> unit
val read_width_height : Td.game_state -> string -> unit
val debug_show_planet : Td.planet -> unit
val parse_map : Td.game_state -> string -> unit
val send_string : string -> unit
val done_sending : unit -> unit
val serialize_move : Td.move  -> string
val send_moves : Td.move list -> unit
