(* OCaml Starter for Halite II on Halite.io
   This code is public domain. There is no warranty.
 *)


open Td;;

(* Input processing *)

(* tokenizer from rosetta code *)
(* String.split_on_char not introduced until OCaml 4.04 *)
let split_char sep str =
  let string_index_from i =
    try Some (String.index_from str i sep)
    with Not_found -> None
  in
  let rec aux i acc = match string_index_from i with
    | Some i' ->
        let w = String.sub str i (i' - i) in
        aux (succ i') (w::acc)
    | None ->
        let w = String.sub str i (String.length str - i) in
        List.rev (w::acc)
  in
  aux 0 []
;;

let get_string () =
  String.trim (read_line ())
;;

let set_width_height state width height =
  state.width <- width;
  state.height <- height;
;;

let read_width_height state s =
  Scanf.sscanf s "%i %i" (set_width_height state)
;;

let docking_status_of_string = function
  | "0" -> `Undocked
  | "1" -> `Docking
  | "2" -> `Docked
  | "3" -> `Undocking
  | s -> 
    (* Log the error but return a status rather than breaking input process *)
    Debug.error ("Invalid input to docking_status_of_string: " ^ s);
    `Undocked 
;;

let parse_ship state player_id tokens =
  match tokens with
  | sid_s :: x_s :: y_s :: hp_s :: vel_x_s :: vel_y_s :: docked_s :: docked_planet_s :: progress_s :: cooldown_s :: tail ->
    let ship_id = int_of_string sid_s in
    let docked = docking_status_of_string docked_s in
    let x = float_of_string x_s in
    let y = float_of_string y_s in
    let hp = int_of_string hp_s in
    let vel_x = float_of_string vel_x_s in
    let vel_y = float_of_string vel_y_s in
    let docked_planet = int_of_string docked_planet_s in
    let progress = int_of_string progress_s in
    let cooldown = int_of_string cooldown_s in
    let ship = Utility.new_ship player_id ship_id x y hp vel_x vel_y 
      docked docked_planet progress cooldown
    in
      state.player.(player_id).ships <- ship :: state.player.(player_id).ships;
      state.all_ships <- ship :: state.all_ships;
      (*
      if docked = `Docked then
        add_ship_to_planet_dock state ship;
      *)
      tail
  | _ -> failwith "parse_ship failed: not enough tokens"
;;

let rec parse_n_ships state player_id n tokens =
  match n with
  | 0 -> tokens
  | num ->
    let remainder = parse_ship state player_id tokens in
      parse_n_ships state player_id (num - 1) remainder
;;

let parse_ships state player_id tokens =
  match tokens with
  | num_ships_string :: ships ->
    let num_ships = int_of_string num_ships_string in
    let remainder = parse_n_ships state player_id num_ships ships in
      remainder
  | [] -> failwith "Empty list provided to parse_ships"
;;

(* This function is just a wrapper around parse_ships *)
let parse_player state player_id tokens =
  let remainder = parse_ships state player_id tokens in
    remainder
;;

let rec parse_n_players state n tokens =
  match n with
  | 0 -> tokens
  | num ->
    begin match tokens with
    | player_id :: tail ->
      let remainder = parse_player state (int_of_string player_id) tail in
        parse_n_players state (num - 1) remainder
    | [] -> failwith "parse_n_players ran out of inputs"
    end
;;

let new_player_array num =
  let count = ref (-1) in
    Array.map (fun _ ->
      count := !count + 1;
      {
        player_id = !count;
        ships = [];
      }
    ) (Array.make num 0)
;;

let parse_player_tokens state tokens =
  match tokens with
  | num_players :: players ->
    state.all_ships <- [];
    state.player <- new_player_array (int_of_string num_players);
    parse_n_players state (int_of_string num_players) players
  | [] -> failwith "parse_player_tokens began empty"
;;

let get_docked_ship_ids num tokens =
  let remainder = ref [] in
  let rec get_next_ship num l =
    match num with
    | 0 -> 
      remainder := l; 
      []
    | _ ->
      begin match l with
      | ship_id_s :: tail ->
        int_of_string ship_id_s :: (get_next_ship (num - 1)) tail
      | [] -> failwith "get_docked_ship_ids failed: not enough tokens"
      end
  in
    let ship_ids = get_next_ship num tokens in
    ship_ids, !remainder
;;

let bool_of_intstring = function
| "0" -> false
| _ -> true
;;

let debug_show_planet planet =
  let p = planet.p_entity in
    Debug.debug (Printf.sprintf 
      "Planet id\t%i\n x\t\t%f\ny\t\t%f\nradius\t\t%f\nhealth\t\t%i\nowner\t\t%i\n\n" 
      p.id p.x p.y p.radius p.health p.owner)
;;

let parse_planet state planet_id tokens =
  match tokens with
  | x_s :: y_s :: hp_s :: r_s :: docking_s :: current_s :: remaining_s :: owned_s :: owner_s :: num_docked_ships_s :: tail ->
    let num_docked_ships = int_of_string num_docked_ships_s in
    let docked_ship_ids, remainder = 
      get_docked_ship_ids num_docked_ships tail 
    in
    let planet = Utility.new_planet
        planet_id
        (float_of_string x_s)
        (float_of_string y_s)
        (int_of_string hp_s)
        (float_of_string r_s)
        (int_of_string docking_s)
        (int_of_string current_s)
        (int_of_string remaining_s)
        (bool_of_intstring owned_s)
        (int_of_string owner_s)
        docked_ship_ids
    in
      planet, remainder
  | _ -> failwith "parse_planet failed: not enough tokens."
;;

let rec parse_n_planets state n tokens =
  match n with
  | 0 -> tokens
  | num ->
    begin match tokens with
    | planet_id_s :: tail ->
      let planet, remainder = parse_planet state (int_of_string planet_id_s) tail in
        state.planets <- planet :: state.planets;
        parse_n_planets state (num - 1) remainder
    | [] -> failwith "parse_n_planets ran out of inputs"
    end
;;

let parse_planet_tokens state tokens =
  (* Clear the planet list, unless we want to write the logic to update 
   * existing planet data.
  *)
  state.planets <- []; 
  match tokens with
  | num_planets :: planets ->
    parse_n_planets state (int_of_string num_planets) planets
  | [] -> failwith "parse_planet_tokens began empty"
;;

let parse_map state s =
  let tokens = split_char ' ' s in
  let planets = parse_player_tokens state tokens in
  let tail = parse_planet_tokens state planets in 
    assert (List.length tail = 0);
    (* Put planets in ascending order *)
    state.planets <- List.sort (fun p1 p2 ->
      p1.p_entity.id - p2.p_entity.id
    ) state.planets
;;

(* End input section *)


(* Output *)

let send_string s = 
  Printf.printf "%s" s
;;

let done_sending () =
  send_string "\n";
  flush stdout;
;;

let serialize_move = function
  | `Thrust (id, magnitude, angle) -> 
    Printf.sprintf "t %i %i %i" id magnitude angle
  | `Dock (ship_id, planet_id) ->
    Printf.sprintf "d %i %i" ship_id planet_id
  | `Undock ship_id ->
    Printf.sprintf "u %i" ship_id
;;

let send_moves moves =
  List.iter (fun move ->
    send_string (serialize_move move)
  ) moves;
  done_sending ()
;;

(* End Output section *)


